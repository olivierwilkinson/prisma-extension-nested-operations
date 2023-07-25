import { Prisma } from "@prisma/client";
import { Types } from "@prisma/client/runtime/library";
import get from "lodash/get";

import {
  LogicalOperator,
  NestedParams,
  NestedWriteOperation,
  Target,
} from "../types";

import {
  isWriteOperation,
  logicalOperators,
  modifiers,
  readOperations,
} from "./operations";
import { findOppositeRelation, relationsByModel } from "./relations";

type NestedOperationInfo<
  ExtArgs extends Types.Extensions.InternalArgs = Types.Extensions.DefaultArgs
> = {
  params: NestedParams<ExtArgs>;
  target: Target;
};

// actions have nested relations inside fields within the args object, sometimes
// relations are defined directly in the args object because the action is in a
// to one relation, for example the update action. Add undefined for actions where this
// can happen
export const fieldsByWriteOperation: Record<
  NestedWriteOperation,
  (string | undefined)[]
> = {
  create: [undefined, "data"],
  update: [undefined, "data"],
  upsert: ["update", "create"],
  connectOrCreate: ["create"],
  createMany: ["data"],
  updateMany: ["data"],
  connect: [],
  disconnect: [],
  delete: [],
  deleteMany: [],
};

export function extractRelationLogicalWhereOperations<
  ExtArgs extends Types.Extensions.InternalArgs = Types.Extensions.DefaultArgs
>(
  params: NestedParams<ExtArgs>,
  parentTarget?: Target,
  parentOperations: { logicalOperator: LogicalOperator; index?: number }[] = []
): NestedOperationInfo[] {
  const relations = relationsByModel[params.model || ""] || [];
  const nestedWhereOperations: NestedOperationInfo[] = [];

  const operationsPath: string[] = [];
  parentOperations.forEach(({ logicalOperator, index }) => {
    operationsPath.push(logicalOperator);

    if (typeof index === "number") {
      operationsPath.push(index.toString());
    }
  });

  logicalOperators.forEach((logicalOperator) => {
    const baseArgPath = params.scope ? ["args"] : ["args", "where"];
    const logicalArg = get(params, [
      ...baseArgPath,
      ...operationsPath,
      logicalOperator,
    ]);
    if (!logicalArg) return;

    const nestedOperators = Array.isArray(logicalArg)
      ? logicalArg.map((_, index) => ({ logicalOperator, index }))
      : [{ logicalOperator }];

    nestedOperators.forEach((nestedOperator) => {
      nestedWhereOperations.push(
        ...extractRelationLogicalWhereOperations(params, parentTarget, [
          ...parentOperations,
          nestedOperator,
        ])
      );
    });

    relations.forEach((relation) => {
      const model = relation.type as Prisma.ModelName;
      const oppositeRelation = findOppositeRelation(relation);

      if (Array.isArray(logicalArg)) {
        logicalArg.forEach((where, index) => {
          const arg = where?.[relation.name];
          if (!arg) return;

          const logicalOperations = [
            ...parentOperations,
            { logicalOperator, index },
          ];
          const foundModifiers = modifiers.filter((mod) => arg[mod]);

          // if there are no modifiers call the where action without a modifier
          if (!foundModifiers.length) {
            nestedWhereOperations.push({
              target: {
                operation: "where" as const,
                relationName: relation.name,
                logicalOperations,
                parentTarget,
              },
              params: {
                model,
                operation: "where",
                args: arg,
                scope: {
                  parentParams: params,
                  logicalOperators: logicalOperations.map(
                    (op) => op.logicalOperator
                  ),
                  relations: { to: relation, from: oppositeRelation },
                },
                query: params.query,
              },
            });

            return;
          }

          // if there are modifiers call the where action with each modifier but
          // not the action without a modifier
          foundModifiers.forEach((modifier) => {
            nestedWhereOperations.push({
              target: {
                operation: "where" as const,
                relationName: relation.name,
                modifier,
                logicalOperations,
                parentTarget,
              },
              params: {
                model,
                operation: "where",
                args: arg[modifier],
                scope: {
                  parentParams: params,
                  modifier,
                  logicalOperators: logicalOperations.map(
                    (op) => op.logicalOperator
                  ),
                  relations: { to: relation, from: oppositeRelation },
                },
                query: params.query,
              },
            });
          });
        });

        return;
      }

      const arg = logicalArg[relation.name];
      if (!arg) return;

      const logicalOperations = [...parentOperations, { logicalOperator }];
      const foundModifiers = modifiers.filter((mod) => arg[mod]);

      if (!foundModifiers.length) {
        nestedWhereOperations.push({
          target: {
            operation: "where",
            relationName: relation.name,
            logicalOperations,
            parentTarget,
          },
          params: {
            model,
            operation: "where",
            args: arg,
            scope: {
              parentParams: params,
              logicalOperators: logicalOperations.map(
                (op) => op.logicalOperator
              ),
              relations: { to: relation, from: oppositeRelation },
            },
            query: params.query,
          },
        });

        return;
      }

      foundModifiers.forEach((modifier) => {
        nestedWhereOperations.push({
          target: {
            operation: "where",
            relationName: relation.name,
            modifier,
            logicalOperations,
            parentTarget,
          },
          params: {
            model,
            operation: "where",
            args: modifier ? arg[modifier] : arg,
            scope: {
              parentParams: params,
              modifier,
              logicalOperators: logicalOperations.map(
                (op) => op.logicalOperator
              ),
              relations: { to: relation, from: oppositeRelation },
            },
            query: params.query,
          },
        });
      });
    });
  });

  return nestedWhereOperations;
}

export function extractRelationWhereOperations<
  ExtArgs extends Types.Extensions.InternalArgs = Types.Extensions.DefaultArgs
>(params: NestedParams<ExtArgs>, parentTarget?: Target): NestedOperationInfo[] {
  const relations = relationsByModel[params.model || ""] || [];

  const nestedWhereOperations = extractRelationLogicalWhereOperations(
    params,
    parentTarget
  );

  relations.forEach((relation) => {
    const model = relation.type as Prisma.ModelName;
    const oppositeRelation = findOppositeRelation(relation);

    const baseArgPath = params.scope ? ["args"] : ["args", "where"];
    const arg = get(params, [...baseArgPath, relation.name]);
    if (!arg) return;

    const foundModifiers = modifiers.filter((mod) => arg[mod]);
    if (!foundModifiers.length) {
      nestedWhereOperations.push({
        target: {
          operation: "where",
          relationName: relation.name,
          parentTarget,
        },
        params: {
          model,
          operation: "where",
          args: arg,
          scope: {
            parentParams: params,
            relations: { to: relation, from: oppositeRelation },
          },
          query: params.query,
        },
      });

      return;
    }

    foundModifiers.forEach((modifier) => {
      nestedWhereOperations.push({
        target: {
          operation: "where",
          relationName: relation.name,
          modifier,
          parentTarget,
        },
        params: {
          model,
          operation: "where",
          args: modifier ? arg[modifier] : arg,
          scope: {
            parentParams: params,
            modifier,
            relations: { to: relation, from: oppositeRelation },
          },
          query: params.query,
        },
      });
    });
  });

  return nestedWhereOperations.concat(
    nestedWhereOperations.flatMap((nestedOperationInfo) =>
      extractRelationWhereOperations(
        nestedOperationInfo.params,
        nestedOperationInfo.target
      )
    )
  );
}

export function extractRelationWriteOperations<
  ExtArgs extends Types.Extensions.InternalArgs = Types.Extensions.DefaultArgs
>(params: NestedParams<ExtArgs>, parentTarget?: Target): NestedOperationInfo[] {
  const relations = relationsByModel[params.model || ""] || [];

  if (!isWriteOperation(params.operation)) return [];

  const nestedWriteOperations: NestedOperationInfo[] = [];
  const fields = fieldsByWriteOperation[params.operation] || [];

  relations.forEach((relation) => {
    const model = relation.type as Prisma.ModelName;
    const oppositeRelation = findOppositeRelation(relation);

    fields.forEach((field) => {
      const argPath = ["args", field, relation.name].filter(
        (part): part is string => !!part
      );
      const arg = get(params, argPath, {});

      Object.keys(arg)
        .filter(isWriteOperation)
        .forEach((operation) => {
          /*
            Add single writes passed as a list as separate operations.
  
            Checking if the operation is an array is enough since only lists of
            separate operations are passed as arrays at the top level. For example
            a nested create may be passed as an array but a nested createMany will
            pass an object with a data array.
          */
          if (Array.isArray(arg[operation])) {
            nestedWriteOperations.push(
              ...arg[operation].map(
                (item: any, index: number): NestedOperationInfo => ({
                  target: {
                    field,
                    relationName: relation.name,
                    operation,
                    index,
                    parentTarget,
                  },
                  params: {
                    model,
                    operation,
                    args: item,
                    scope: {
                      parentParams: params,
                      relations: { to: relation, from: oppositeRelation },
                    },
                    query: params.query,
                  },
                })
              )
            );
            return;
          }

          nestedWriteOperations.push({
            target: {
              field,
              relationName: relation.name,
              operation,
              parentTarget,
            },
            params: {
              model,
              operation,
              args: arg[operation],
              scope: {
                parentParams: params,
                relations: { to: relation, from: oppositeRelation },
              },
              query: params.query,
            },
          });
        });
    });
  });

  return nestedWriteOperations.concat(
    nestedWriteOperations.flatMap((nestedOperationInfo) =>
      extractRelationWriteOperations(
        nestedOperationInfo.params,
        nestedOperationInfo.target
      )
    )
  );
}

export function extractRelationReadOperations<
  ExtArgs extends Types.Extensions.InternalArgs = Types.Extensions.DefaultArgs
>(params: NestedParams<ExtArgs>, parentTarget?: Target): NestedOperationInfo[] {
  const relations = relationsByModel[params.model || ""] || [];
  const nestedOperations: NestedOperationInfo[] = [];

  relations.forEach((relation) => {
    const model = relation.type as Prisma.ModelName;
    const oppositeRelation = findOppositeRelation(relation);

    readOperations.forEach((operation) => {
      const arg = get(params, ["args", operation, relation.name]);
      if (!arg) return;

      const readOperationInfo = {
        params: {
          model,
          operation,
          args: arg,
          scope: {
            parentParams: params,
            relations: { to: relation, from: oppositeRelation },
          },
          // this needs to be nested query function
          query: params.query,
        },
        target: { operation, relationName: relation.name, parentTarget },
      };

      nestedOperations.push(readOperationInfo);

      if (readOperationInfo.params.args?.where) {
        const whereOperationInfo = {
          target: {
            operation: "where" as const,
            relationName: relation.name,
            readOperation: operation,
            parentTarget: readOperationInfo.target,
          },
          params: {
            model: readOperationInfo.params.model,
            operation: "where" as const,
            args: readOperationInfo.params.args.where,
            scope: {
              parentParams: readOperationInfo.params,
              relations: readOperationInfo.params.scope.relations,
            },
            query: params.query,
          },
        };
        nestedOperations.push(whereOperationInfo);
        nestedOperations.push(
          ...extractRelationWhereOperations(
            whereOperationInfo.params,
            whereOperationInfo.target
          )
        );
      }

      // push select nested in an include
      if (operation === "include" && arg.select) {
        const nestedSelectOperationInfo = {
          params: {
            model,
            operation: "select" as const,
            args: arg.select,
            scope: {
              parentParams: readOperationInfo.params,
              relations: readOperationInfo.params.scope.relations,
            },
            query: params.query,
          },
          target: {
            field: "include" as const,
            operation: "select" as const,
            relationName: relation.name,
            parentTarget,
          },
        };

        nestedOperations.push(nestedSelectOperationInfo);

        if (nestedSelectOperationInfo.params.args?.where) {
          const whereOperationInfo = {
            target: {
              operation: "where" as const,
              relationName: relation.name,
              readOperation: "select" as const,
              parentTarget: nestedSelectOperationInfo.target,
            },
            params: {
              model: nestedSelectOperationInfo.params.model,
              operation: "where" as const,
              args: nestedSelectOperationInfo.params.args.where,
              scope: {
                parentParams: nestedSelectOperationInfo.params,
                relations: nestedSelectOperationInfo.params.scope.relations,
              },
              query: params.query,
            },
          };
          nestedOperations.push(whereOperationInfo);
          nestedOperations.push(
            ...extractRelationWhereOperations(
              whereOperationInfo.params,
              whereOperationInfo.target
            )
          );
        }
      }
    });
  });

  return nestedOperations.concat(
    nestedOperations.flatMap((nestedOperation) =>
      extractRelationReadOperations(
        nestedOperation.params,
        nestedOperation.target
      )
    )
  );
}

export function extractNestedOperations<
  ExtArgs extends Types.Extensions.InternalArgs = Types.Extensions.DefaultArgs
>(params: NestedParams<ExtArgs>): NestedOperationInfo[] {
  return [
    ...extractRelationWhereOperations(params),
    ...extractRelationReadOperations(params),
    ...extractRelationWriteOperations(params),
  ];
}
