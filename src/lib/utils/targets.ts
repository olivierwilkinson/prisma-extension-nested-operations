import {
  LogicalOperator,
  QueryTarget,
  ReadTarget,
  Target,
  WriteTarget,
} from "../types";

import { isQueryOperation, isReadOperation, isWriteOperation } from "./operations";

export function isQueryTarget(target: any): target is QueryTarget {
  return isQueryOperation(target.operation);
}

export function isReadTarget(target: any): target is ReadTarget {
  return isReadOperation(target.operation);
}

export function isWriteTarget(target: any): target is WriteTarget {
  return isWriteOperation(target.operation);
}

export function buildOperationsPath(
  operations?: { logicalOperator: LogicalOperator; index?: number }[]
) {
  if (!operations) return [];

  return operations.flatMap((op) => {
    if (typeof op.index === "number")
      return [op.logicalOperator, op.index.toString()];

    return [op.logicalOperator];
  });
}

export function buildQueryTargetPath(target: QueryTarget): string[] {
  const path = target.parentTarget
    ? buildTargetPath(target.parentTarget)
    : [];

  if (!target.relationName) {
    return [...path, target.operation];
  }

  if (target.logicalOperations) {
    path.push(...buildOperationsPath(target.logicalOperations));
  }

  if (target.readOperation) {
    path.push(target.readOperation);
  }

  path.push(target.relationName);

  if (target.readOperation) {
    path.push("where");
  }

  if (target.modifier) {
    path.push(target.modifier);
  }

  return path;
}

export function buildWriteTargetPath(target: WriteTarget): string[] {
  const path = target.parentTarget ? buildTargetPath(target.parentTarget) : [];

  if (target.field) {
    path.push(target.field);
  }

  path.push(target.relationName, target.operation);

  if (typeof target.index === "number") {
    path.push(target.index.toString());
  }

  return path;
}

export function buildReadTargetPath(target: ReadTarget): string[] {
  const path = target.parentTarget ? buildTargetPath(target.parentTarget) : [];

  if (!target.relationName) {
    return [...path, target.operation];
  }

  if (!target.field) {
    return [...path, target.operation, target.relationName];
  }

  return [...path, target.field, target.relationName, target.operation];
}

export function buildTargetPath(target: Target) {
  if (isQueryTarget(target)) return buildQueryTargetPath(target);
  if (isReadTarget(target)) return buildReadTargetPath(target);
  return buildWriteTargetPath(target);
}

export const buildTargetRelationPath = (target: Target): string[] | null => {
  if (!isReadTarget(target)) return null;

  if (target.parentTarget) {
    const basePath = buildTargetRelationPath(target.parentTarget);
    if (!basePath) return null;

    return target.relationName ? [...basePath, target.relationName] : basePath;
  }

  return target.relationName ? [target.relationName] : [];
};

export function targetChainLength(target: Target, count = 0): number {
  if (!target.parentTarget) {
    return count + 1;
  }
  return targetChainLength(target.parentTarget, count + 1);
}
