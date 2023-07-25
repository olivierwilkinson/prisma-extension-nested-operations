import { Types } from "@prisma/client/runtime/library";
import { DeferredPromise } from "@open-draft/deferred-promise";
import { omit } from "lodash";

import { ExecuteFunction, NestedParams, OperationCall, Target } from "../types";
import { cloneArgs } from "./cloneArgs";

export async function executeOperation<ExtArgs extends Types.Extensions.InternalArgs = Types.Extensions.DefaultArgs>(
  execute: ExecuteFunction,
  params: NestedParams<ExtArgs>,
  target: Target
): Promise<OperationCall<ExtArgs>> {
  const queryCalledPromise = new DeferredPromise<any>();
  const queryPromise = new DeferredPromise<any>();

  const result = execute({
    ...cloneArgs(params),
    query: (updatedArgs, updatedOperation = params.operation) => {
      queryCalledPromise.resolve({
        updatedArgs,
        updatedOperation
      });
      return queryPromise;
    },
  }).catch((e) => {
    // reject params updated callback so it throws when awaited
    queryCalledPromise.reject(e);

    // if next has already been resolved we must throw
    if (queryPromise.state === "fulfilled") {
      throw e;
    }
  });

  const { updatedArgs, updatedOperation } = await queryCalledPromise;

  // execute middleware with updated params if action has changed
  if (updatedOperation !== params.operation) {
    return executeOperation(
      execute,
      {
        ...params,
        operation: updatedOperation,
        args: updatedArgs,
      },
      omit(target, "index") as Target
    );
  }

  // execute middleware with updated params if action has changed
  return {
    queryPromise,
    result,
    updatedArgs,
    origin: target,
    target: { ...target, operation: params.operation as any },
    scope: params.scope,
  };
}
