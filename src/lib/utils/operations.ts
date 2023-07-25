import {
  LogicalOperator,
  Modifier,
  NestedQueryOperation,
  NestedReadOperation,
  NestedWriteOperation,
} from "../types";

export const queryOperations: NestedQueryOperation[] = ["where"];
export const readOperations: NestedReadOperation[] = ["include", "select"];
export const writeOperations: NestedWriteOperation[] = [
  "create",
  "update",
  "upsert",
  "createMany",
  "updateMany",
  "delete",
  "deleteMany",
  "disconnect",
  "connect",
  "connectOrCreate",
];
export const toOneRelationNonListOperations: NestedWriteOperation[] = [
  "create",
  "update",
  "delete",
  "upsert",
  "connect",
  "connectOrCreate",
  "disconnect",
];

export function isQueryOperation(action: any): action is NestedQueryOperation {
  return queryOperations.includes(action);
}

export function isReadOperation(action: any): action is NestedReadOperation {
  return readOperations.includes(action);
}

export function isWriteOperation(action: any): action is NestedWriteOperation {
  return writeOperations.includes(action);
}

export const modifiers: Modifier[] = ["is", "isNot", "some", "none", "every"];
export const logicalOperators: LogicalOperator[] = ["AND", "OR", "NOT"];
