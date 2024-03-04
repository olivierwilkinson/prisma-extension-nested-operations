import { Types } from "@prisma/client/runtime/library";
import merge from "lodash/merge";
import omit from "lodash/omit";
import get from "lodash/get";
import set from "lodash/set";
import unset from "lodash/unset";

import {
  OperationCall,
  NestedOperation,
  Target,
  WriteTarget,
  Scope,
  ReadTarget,
  NestedWriteOperation,
  NestedParams,
} from "../types";

import {
  buildQueryTargetPath,
  buildReadTargetPath,
  buildTargetPath,
  buildWriteTargetPath,
  isQueryTarget,
  isReadTarget,
  isWriteTarget,
  targetChainLength,
} from "./targets";
import {
  isQueryOperation,
  isReadOperation,
  isWriteOperation,
  toOneRelationNonListOperations,
} from "./operations";
import { cloneArgs } from "./cloneArgs";
import { fieldsByWriteOperation } from "./extractNestedOperations";

function addWriteToArgs<
  ExtArgs extends Types.Extensions.InternalArgs = Types.Extensions.DefaultArgs
>(args: any, updatedArgs: any, target: WriteTarget, scope?: Scope<ExtArgs>) {
  const toOneRelation = !scope?.relations.to.isList;
  const targetPath = buildWriteTargetPath(target);
  const targetArgs = get(args, targetPath);

  // it's possible to target args that have already been updated if the user
  // has reused the same object in multiple places when changing action, in this
  // case we can just return
  if (targetArgs === updatedArgs) {
    return;
  }

  // if target doesn't exist or is a boolean action, we can just set the args
  if (!targetArgs || typeof targetArgs === "boolean") {
    set(args, targetPath, updatedArgs);
    return;
  }

  // createMany operations cannot be turned into arrays of operations so merge
  // their data fields
  if (target.operation === "createMany") {
    set(
      args,
      [...targetPath, "data"],
      [...targetArgs.data, ...updatedArgs.data]
    );
    return;
  }

  // to one relations have actions that cannot be turned into arrays of operations
  // so merge their args
  if (
    toOneRelation &&
    toOneRelationNonListOperations.includes(target.operation)
  ) {
    merge(get(args, targetPath), updatedArgs);
    return;
  }

  // if target is an array of operations push args as another operation
  if (Array.isArray(targetArgs)) {
    targetArgs.push(updatedArgs);
    return;
  }

  // convert target to an array of operations with the target args as the
  // first operation and passed args as the second
  set(args, targetPath, [targetArgs, updatedArgs]);
}

function removeWriteFromArgs(args: any, target: WriteTarget) {
  // remove args from target
  const targetPath = buildWriteTargetPath(target);
  unset(args, targetPath);

  // if target parent is now an empty object or array we must remove it
  const targetParentPath = targetPath.slice(0, -1);
  const targetParent = get(args, targetParentPath);
  if (Object.keys(targetParent).length === 0) {
    unset(args, targetParentPath);
  }
}

function removeReadFromArgs(args: any, target: ReadTarget) {
  // remove args from target
  const targetPath = buildReadTargetPath(target);
  unset(args, targetPath);

  // if target parent is an array with only unset values we must remove it
  const targetParentPath = targetPath.slice(0, -1);
  const targetParent = get(args, targetParentPath);
  if (Object.keys(targetParent).length === 0) {
    unset(args, targetParentPath);
  }
}

export function assertOperationChangeIsValid(
  previousOperation: NestedOperation,
  nextOperation: NestedOperation
) {
  if (isReadOperation(previousOperation) && isWriteOperation(nextOperation)) {
    throw new Error(
      "Changing a read action to a write action is not supported"
    );
  }

  if (isWriteOperation(previousOperation) && isReadOperation(nextOperation)) {
    throw new Error(
      "Changing a write action to a read action is not supported"
    );
  }

  if (isQueryOperation(previousOperation) && !isQueryOperation(nextOperation)) {
    throw new Error(
      "Changing a query action to a non-query action is not supported"
    );
  }
}

function moveOperationChangesToEnd(
  callA: { target: Target; origin: Target },
  callB: { target: Target; origin: Target }
) {
  if (callA.target.operation !== callA.origin.operation) {
    return 1;
  }
  if (callB.target.operation !== callB.origin.operation) {
    return -1;
  }
  return 0;
}

function findParentCall<Call extends { origin: Target }>(
  calls: Call[],
  origin: Target
): Call | undefined {
  return calls.find(
    (call) =>
      origin.parentTarget &&
      buildTargetPath(origin.parentTarget).join(".") ===
        buildTargetPath(call.origin).join(".")
  );
}

export function buildArgsFromCalls<
  ExtArgs extends Types.Extensions.InternalArgs,
  Call extends Omit<OperationCall<ExtArgs>, "queryPromise" | "result">
>(calls: Call[], rootParams: NestedParams<ExtArgs>) {
  const finalArgs = cloneArgs(rootParams.args);

  // calls should update the parent calls updated params

  // sort calls so we set from deepest to shallowest
  // actions that are at the same depth should put action changes at the end
  const sortedCalls = calls.sort((a, b) => {
    const aDepth = targetChainLength(a.target);
    const bDepth = targetChainLength(b.target);

    if (aDepth === bDepth) {
      return moveOperationChangesToEnd(a, b);
    }

    return bDepth - aDepth;
  });

  // eslint-disable-next-line complexity
  sortedCalls.forEach((call, i) => {
    const parentCall = findParentCall(calls.slice(i), call.origin);
    const parentArgs = parentCall?.updatedArgs || finalArgs;
    const parentOperation =
      parentCall?.target.operation || rootParams.operation;

    const origin = omit(call.origin, "parentTarget");
    const target = omit(call.target, "parentTarget");

    if (origin.operation !== target.operation) {
      assertOperationChangeIsValid(origin.operation, target.operation);
    }

    if (isWriteTarget(target) && isWriteTarget(origin)) {
      // if action has not changed use normal target to set args
      if (target.operation === origin.operation) {
        const targetPath = buildWriteTargetPath(target);
        const callTargetArgs = get(parentArgs, targetPath);

        // if target hasn't changed but is an array it has been merged
        // the original target must be the first element of the array
        if (Array.isArray(callTargetArgs)) {
          callTargetArgs[0] = call.updatedArgs;
          return;
        }

        // set the updated args if the target hasn't changed
        set(parentArgs, targetPath, call.updatedArgs);
        return;
      }

      // if parent action has not changed we can use our normal targets
      if (parentOperation === call.scope?.parentParams.operation) {
        addWriteToArgs(parentArgs, call.updatedArgs, target, call.scope);
        removeWriteFromArgs(parentArgs, origin);
        return;
      }

      // if parent action has changed we must modify out target to match the
      // parent action
      const fields =
        // NOTE:- this might need to be origin.operation
        fieldsByWriteOperation[target.operation as NestedWriteOperation];

      fields.forEach((field) => {
        const newOrigin = { ...origin, field };
        const newTarget = { ...target, field };

        if (get(parentArgs, buildWriteTargetPath(newOrigin))) {
          // if action has changed we add merge args with target and remove the
          // args from the origin
          addWriteToArgs(parentArgs, call.updatedArgs, newTarget, call.scope);
          removeWriteFromArgs(parentArgs, newOrigin);
        }
      });
    }

    if (isReadTarget(target) && isReadTarget(origin)) {
      const targetPath = buildReadTargetPath(target);
      // because includes and selects cannot be at the same level we can safely
      // set target path to be the updated args without worrying about
      // overwriting the original args
      set(parentArgs, targetPath, call.updatedArgs);

      // remove the origin args if the action has changed
      if (target.operation !== origin.operation) {
        removeReadFromArgs(parentArgs, origin);
      }
    }

    if (isQueryTarget(target) && isQueryTarget(origin)) {
      if (target.readOperation) {
        set(parentArgs, "where", call.updatedArgs);
        return;
      }

      const basePath = parentCall ? [] : ["where"];
      set(
        parentArgs,
        [...basePath, ...buildQueryTargetPath(target)],
        call.updatedArgs
      );
    }
  });

  return finalArgs;
}
