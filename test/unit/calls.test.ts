import { Prisma } from "@prisma/client";
import { Types } from "@prisma/client/runtime/library";
import faker from "faker";
import get from "lodash/get";

import { withNestedOperations, NestedParams } from "../../src";
import { relationsByModel } from "../../src/lib/utils/relations";
import { LogicalOperator, Modifier } from "../../src/lib/types";
import { createParams } from "./helpers/createParams";

type OperationCall<Model extends Prisma.ModelName> = {
  model: Model;
  operation:
    | "create"
    | "update"
    | "upsert"
    | "delete"
    | "createMany"
    | "updateMany"
    | "deleteMany"
    | "connectOrCreate"
    | "findUnique"
    | "findFirst"
    | "findMany"
    | "include"
    | "select"
    | "where"
    | "groupBy";
  argsPath: string;
  scope?: OperationCall<any>;
  relations: {
    to: Prisma.DMMF.Field;
    from: Prisma.DMMF.Field;
  };
  modifier?: Modifier;
  logicalOperators?: LogicalOperator[];
};

function nestedParamsFromCall<Model extends Prisma.ModelName,
ExtArgs extends Types.Extensions.InternalArgs = Types.Extensions.DefaultArgs
>(
  rootParams: NestedParams<ExtArgs>,
  call: OperationCall<Model>
): NestedParams<ExtArgs> {
  const params = createParams(
    query,
    call.model,
    call.operation,
    get(rootParams, call.argsPath)
  );
  return {
    ...params,
    scope: {
      modifier: call.modifier,
      logicalOperators: call.logicalOperators,
      relations: call.relations,
      parentParams: call.scope
        ? nestedParamsFromCall(rootParams, call.scope)
        : rootParams,
    },
  };
}

function getModelRelation<Model extends Prisma.ModelName>(
  model: Model,
  relationName: string
): Prisma.DMMF.Field {
  const modelRelation = relationsByModel[model].find(
    (relation) => relation.name === relationName
  );
  if (!modelRelation) {
    throw new Error(
      `Unable to find relation ${relationName} on model ${model}`
    );
  }
  return modelRelation;
}

const query = (_: any) => Promise.resolve({});

describe("calls", () => {
  it("calls middleware once when there are no nested operations", async () => {
    const $rootOperation = jest.fn((params) => params.query(params.args));
    const $allNestedOperations = jest.fn((params) => params.query(params.args));
    const allOperations = withNestedOperations({
      $rootOperation,
      $allNestedOperations,
    });

    const params = createParams(query, "User", "create", {
      data: { email: faker.internet.email() },
    });
    await allOperations(params);

    // middleware is called with params and next
    expect($rootOperation).toHaveBeenCalledTimes(1);
    expect($rootOperation).toHaveBeenCalledWith({
      ...params,
      query: expect.any(Function),
    });
    expect($allNestedOperations).not.toHaveBeenCalled();
  });

  it.each<{
    description: string;
    rootParams: NestedParams<Types.Extensions.DefaultArgs>;
    nestedCalls?: OperationCall<any>[];
  }>([
    {
      description: "count",
      rootParams: createParams(query, "User", "count", undefined),
    },
    {
      description: "aggregate",
      rootParams: createParams(query, "User", "aggregate", {}),
    },
    {
      description: "groupBy",
      rootParams: createParams(query, "User", "groupBy", {
        by: ["email"],
        orderBy: { email: "asc" },
      }),
    },
    {
      description: "nested create in create",
      rootParams: createParams(query, "User", "create", {
        data: {
          email: faker.internet.email(),
          profile: { create: { bio: faker.lorem.paragraph() } },
        },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Profile",
          argsPath: "args.data.profile.create",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested create in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: { create: { bio: faker.lorem.paragraph() } },
        },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Profile",
          argsPath: "args.data.profile.create",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested creates in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
          profile: { create: { bio: faker.lorem.paragraph() } },
        },
        update: {
          email: faker.internet.email(),
          profile: { create: { bio: faker.lorem.paragraph() } },
        },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Profile",
          argsPath: "args.create.profile.create",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
        {
          operation: "create",
          model: "Profile",
          argsPath: "args.update.profile.create",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested create array in create",
      rootParams: createParams(query, "User", "create", {
        data: {
          email: faker.internet.email(),
          posts: {
            create: [
              {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
              {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Post",
          argsPath: "args.data.posts.create.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "create",
          model: "Post",
          argsPath: "args.data.posts.create.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested create array in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            create: [
              {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
              {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Post",
          argsPath: "args.data.posts.create.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "create",
          model: "Post",
          argsPath: "args.data.posts.create.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested create and update in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            update: { bio: faker.lorem.paragraph() },
          },
          posts: {
            create: [
              {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Profile",
          argsPath: "args.data.profile.update",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
        {
          operation: "create",
          model: "Post",
          argsPath: "args.data.posts.create.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested create array in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
          posts: {
            create: [
              {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
              {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
            ],
          },
        },
        update: {
          email: faker.internet.email(),
          posts: {
            create: [
              {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
              {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Post",
          argsPath: "args.create.posts.create.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "create",
          model: "Post",
          argsPath: "args.create.posts.create.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "create",
          model: "Post",
          argsPath: "args.update.posts.create.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "create",
          model: "Post",
          argsPath: "args.update.posts.create.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested update in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: { update: { bio: faker.lorem.paragraph() } },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Profile",
          argsPath: "args.data.profile.update",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested update in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          profile: { update: { bio: faker.lorem.paragraph() } },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Profile",
          argsPath: "args.update.profile.update",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested update array in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            update: [
              {
                where: { id: faker.datatype.number() },
                data: {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              },
              {
                where: { id: faker.datatype.number() },
                data: {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.data.posts.update.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "update",
          model: "Post",
          argsPath: "args.data.posts.update.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested update array in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          posts: {
            update: [
              {
                where: { id: faker.datatype.number() },
                data: {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              },
              {
                where: { id: faker.datatype.number() },
                data: {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.update.posts.update.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "update",
          model: "Post",
          argsPath: "args.update.posts.update.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested upsert in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            upsert: {
              create: { bio: faker.lorem.paragraph() },
              update: { bio: faker.lorem.paragraph() },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "upsert",
          model: "Profile",
          argsPath: "args.data.profile.upsert",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested upsert list in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            upsert: [
              {
                where: { id: faker.datatype.number() },
                update: { title: faker.lorem.sentence() },
                create: {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              },
              {
                where: { id: faker.datatype.number() },
                update: { title: faker.lorem.sentence() },
                create: {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "upsert",
          model: "Post",
          argsPath: "args.data.posts.upsert.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "upsert",
          model: "Post",
          argsPath: "args.data.posts.upsert.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested upsert in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          profile: {
            upsert: {
              create: { bio: faker.lorem.paragraph() },
              update: { bio: faker.lorem.paragraph() },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "upsert",
          model: "Profile",
          argsPath: "args.update.profile.upsert",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested delete in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: { delete: true },
        },
      }),
      nestedCalls: [
        {
          operation: "delete",
          model: "Profile",
          argsPath: "args.data.profile.delete",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested delete in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          profile: { delete: true },
        },
      }),
      nestedCalls: [
        {
          operation: "delete",
          model: "Profile",
          argsPath: "args.update.profile.delete",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested delete array in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            delete: [
              { id: faker.datatype.number() },
              { id: faker.datatype.number() },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "delete",
          model: "Post",
          argsPath: "args.data.posts.delete.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "delete",
          model: "Post",
          argsPath: "args.data.posts.delete.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested delete array in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          posts: {
            delete: [
              { id: faker.datatype.number() },
              { id: faker.datatype.number() },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "delete",
          model: "Post",
          argsPath: "args.update.posts.delete.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "delete",
          model: "Post",
          argsPath: "args.update.posts.delete.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested createMany in create",
      rootParams: createParams(query, "User", "create", {
        data: {
          email: faker.internet.email(),
          posts: {
            createMany: {
              data: [
                {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
                {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              ],
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "createMany",
          model: "Post",
          argsPath: "args.data.posts.createMany",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested createMany in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            createMany: {
              data: [
                {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
                {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              ],
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "createMany",
          model: "Post",
          argsPath: "args.data.posts.createMany",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested createMany in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          posts: {
            createMany: {
              data: [
                {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
                {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              ],
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "createMany",
          model: "Post",
          argsPath: "args.update.posts.createMany",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested updateMany in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            updateMany: {
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
              where: { id: faker.datatype.number() },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "updateMany",
          model: "Post",
          argsPath: "args.data.posts.updateMany",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },

    {
      description: "nested updateMany array in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            updateMany: [
              {
                where: {
                  id: faker.datatype.number(),
                },
                data: {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              },
              {
                where: {
                  id: faker.datatype.number(),
                },
                data: {
                  title: faker.lorem.sentence(),
                  content: faker.lorem.paragraph(),
                },
              },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "updateMany",
          model: "Post",
          argsPath: "args.data.posts.updateMany.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "updateMany",
          model: "Post",
          argsPath: "args.data.posts.updateMany.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested updateMany in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          posts: {
            updateMany: {
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
              },
              where: { id: faker.datatype.number() },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "updateMany",
          model: "Post",
          argsPath: "args.update.posts.updateMany",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested updateMany list in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          posts: {
            updateMany: [
              {
                where: { id: faker.datatype.number() },
                data: { title: faker.lorem.sentence() },
              },
              {
                where: { id: faker.datatype.number() },
                data: { title: faker.lorem.sentence() },
              },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "updateMany",
          model: "Post",
          argsPath: "args.update.posts.updateMany.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "updateMany",
          model: "Post",
          argsPath: "args.update.posts.updateMany.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested deleteMany in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            deleteMany: { id: faker.datatype.number() },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "deleteMany",
          model: "Post",
          argsPath: "args.data.posts.deleteMany",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested deleteMany list in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            deleteMany: [
              { id: faker.datatype.number() },
              { id: faker.datatype.number() },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "deleteMany",
          model: "Post",
          argsPath: "args.data.posts.deleteMany.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "deleteMany",
          model: "Post",
          argsPath: "args.data.posts.deleteMany.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested deleteMany in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          posts: {
            deleteMany: { id: faker.datatype.number() },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "deleteMany",
          model: "Post",
          argsPath: "args.update.posts.deleteMany",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested deleteMany list in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          posts: {
            deleteMany: [
              { id: faker.datatype.number() },
              { id: faker.datatype.number() },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "deleteMany",
          model: "Post",
          argsPath: "args.update.posts.deleteMany.0",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "deleteMany",
          model: "Post",
          argsPath: "args.update.posts.deleteMany.1",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested connectOrCreate in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            connectOrCreate: {
              where: { id: faker.datatype.number() },
              create: { bio: faker.lorem.paragraph() },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "connectOrCreate",
          model: "Profile",
          argsPath: "args.data.profile.connectOrCreate",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested connectOrCreate in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          profile: {
            connectOrCreate: {
              where: { id: faker.datatype.number() },
              create: { bio: faker.lorem.paragraph() },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "connectOrCreate",
          model: "Profile",
          argsPath: "args.update.profile.connectOrCreate",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "deeply nested creates",
      rootParams: createParams(query, "User", "create", {
        data: {
          email: faker.internet.email(),
          posts: {
            create: {
              title: faker.lorem.sentence(),
              content: faker.lorem.paragraph(),
              comments: {
                create: {
                  authorId: faker.datatype.number(),
                  content: faker.lorem.paragraph(),
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Post",
          argsPath: "args.data.posts.create",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "create",
          model: "Comment",
          argsPath: "args.data.posts.create.comments.create",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "create",
            model: "Post",
            argsPath: "args.data.posts.create",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
                comments: {
                  update: {
                    where: { id: faker.datatype.number() },
                    data: {
                      authorId: faker.datatype.number(),
                      content: faker.lorem.paragraph(),
                    },
                  },
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.data.posts.update",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "update",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.update",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested delete",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
                comments: {
                  delete: [
                    { id: faker.datatype.number() },
                    { id: faker.datatype.number() },
                  ],
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.data.posts.update",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "delete",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.delete.0",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "delete",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.delete.1",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
                comments: {
                  upsert: {
                    where: { id: faker.datatype.number() },
                    create: {
                      authorId: faker.datatype.number(),
                      content: faker.lorem.paragraph(),
                    },
                    update: {
                      authorId: faker.datatype.number(),
                      content: faker.lorem.paragraph(),
                    },
                  },
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.update.posts.update",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "upsert",
          model: "Comment",
          argsPath: "args.update.posts.update.data.comments.upsert",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.update.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested createMany",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
                comments: {
                  createMany: {
                    data: [
                      {
                        authorId: faker.datatype.number(),
                        content: faker.lorem.paragraph(),
                      },
                      {
                        authorId: faker.datatype.number(),
                        content: faker.lorem.paragraph(),
                      },
                    ],
                  },
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.data.posts.update",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "createMany",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.createMany",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested updateMany",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
                comments: {
                  updateMany: {
                    where: { id: faker.datatype.number() },
                    data: {
                      content: faker.lorem.paragraph(),
                    },
                  },
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.data.posts.update",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "updateMany",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.updateMany",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested updateMany array",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
                comments: {
                  updateMany: [
                    {
                      where: { id: faker.datatype.number() },
                      data: { content: faker.lorem.paragraph() },
                    },
                    {
                      where: { id: faker.datatype.number() },
                      data: { content: faker.lorem.paragraph() },
                    },
                  ],
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.data.posts.update",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "updateMany",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.updateMany.0",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "updateMany",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.updateMany.1",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested deleteMany",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
                comments: {
                  deleteMany: { id: faker.datatype.number() },
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.data.posts.update",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "deleteMany",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.deleteMany",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested deleteMany array",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
                comments: {
                  deleteMany: [
                    { id: faker.datatype.number() },
                    { id: faker.datatype.number() },
                  ],
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.data.posts.update",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "deleteMany",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.deleteMany.0",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "deleteMany",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.deleteMany.1",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested connectOrCreate",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
                content: faker.lorem.paragraph(),
                comments: {
                  connectOrCreate: {
                    where: { id: faker.datatype.number() },
                    create: {
                      authorId: faker.datatype.number(),
                      content: faker.lorem.paragraph(),
                    },
                  },
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "update",
          model: "Post",
          argsPath: "args.data.posts.update",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "connectOrCreate",
          model: "Comment",
          argsPath: "args.data.posts.update.data.comments.connectOrCreate",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "update",
            model: "Post",
            argsPath: "args.data.posts.update",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "include in findUnique",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        include: {
          posts: true,
        },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "include in findFirst",
      rootParams: createParams(query, "User", "findFirst", {
        where: { id: faker.datatype.number() },
        include: { posts: true },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "include in findMany",
      rootParams: createParams(query, "User", "findMany", {
        where: { id: faker.datatype.number() },
        include: { posts: true },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "include in create",
      rootParams: createParams(query, "User", "create", {
        data: {
          email: faker.internet.email(),
          posts: { create: { title: faker.lorem.sentence() } },
        },
        include: { posts: true },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Post",
          argsPath: "args.data.posts.create",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "include in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
        },
        include: { posts: true },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "include in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
        },
        include: { posts: true },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested includes",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        include: {
          posts: {
            include: {
              comments: true,
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "include",
          model: "Comment",
          argsPath: "args.include.posts.include.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested includes",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        include: {
          posts: {
            include: {
              comments: {
                include: {
                  replies: true,
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "include",
          model: "Comment",
          argsPath: "args.include.posts.include.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "include",
          model: "Comment",
          argsPath: "args.include.posts.include.comments.include.replies",
          relations: {
            to: getModelRelation("Comment", "replies"),
            from: getModelRelation("Comment", "repliedTo"),
          },
          scope: {
            operation: "include",
            model: "Comment",
            argsPath: "args.include.posts.include.comments",
            relations: {
              to: getModelRelation("Post", "comments"),
              from: getModelRelation("Comment", "post"),
            },
            scope: {
              operation: "include",
              model: "Post",
              argsPath: "args.include.posts",
              relations: {
                to: getModelRelation("User", "posts"),
                from: getModelRelation("Post", "author"),
              },
            },
          },
        },
      ],
    },
    {
      description: "select in findUnique",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        select: {
          posts: true,
        },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "select in findFirst",
      rootParams: createParams(query, "User", "findFirst", {
        where: { id: faker.datatype.number() },
        select: { posts: true },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "select in findMany",
      rootParams: createParams(query, "User", "findMany", {
        where: { id: faker.datatype.number() },
        select: { posts: true },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "select in create",
      rootParams: createParams(query, "User", "create", {
        data: {
          email: faker.internet.email(),
          posts: { create: { title: faker.lorem.sentence() } },
        },
        select: { posts: true },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Post",
          argsPath: "args.data.posts.create",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "select in update",
      rootParams: createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
        },
        select: { posts: true },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "select in upsert",
      rootParams: createParams(query, "User", "upsert", {
        where: { id: faker.datatype.number() },
        create: {
          email: faker.internet.email(),
        },
        update: {
          email: faker.internet.email(),
        },
        select: { posts: true },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "nested selects",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        select: {
          posts: {
            select: {
              comments: true,
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.select.posts.select.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "select",
            model: "Post",
            argsPath: "args.select.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "deeply nested selects",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        select: {
          posts: {
            select: {
              comments: {
                select: {
                  replies: true,
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.select.posts.select.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "select",
            model: "Post",
            argsPath: "args.select.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.select.posts.select.comments.select.replies",
          relations: {
            to: getModelRelation("Comment", "replies"),
            from: getModelRelation("Comment", "repliedTo"),
          },
          scope: {
            operation: "select",
            model: "Comment",
            argsPath: "args.select.posts.select.comments",
            relations: {
              to: getModelRelation("Post", "comments"),
              from: getModelRelation("Comment", "post"),
            },
            scope: {
              operation: "select",
              model: "Post",
              argsPath: "args.select.posts",
              relations: {
                to: getModelRelation("User", "posts"),
                from: getModelRelation("Post", "author"),
              },
            },
          },
        },
      ],
    },
    {
      description: "deeply nested selects with custom fields",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        select: {
          posts: {
            select: {
              title: true,
              comments: {
                select: {
                  content: true,
                  replies: {
                    select: {
                      content: true,
                      createdAt: true,
                    },
                  },
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.select.posts.select.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "select",
            model: "Post",
            argsPath: "args.select.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.select.posts.select.comments.select.replies",
          relations: {
            to: getModelRelation("Comment", "replies"),
            from: getModelRelation("Comment", "repliedTo"),
          },
          scope: {
            operation: "select",
            model: "Comment",
            argsPath: "args.select.posts.select.comments",
            relations: {
              to: getModelRelation("Post", "comments"),
              from: getModelRelation("Comment", "post"),
            },
            scope: {
              operation: "select",
              model: "Post",
              argsPath: "args.select.posts",
              relations: {
                to: getModelRelation("User", "posts"),
                from: getModelRelation("Post", "author"),
              },
            },
          },
        },
      ],
    },
    {
      description: "nested select in include",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        include: {
          posts: {
            select: {
              comments: true,
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "select",
          model: "Post",
          argsPath: "args.include.posts.select",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.include.posts.select.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "nested include in select",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        select: {
          posts: {
            include: {
              comments: true,
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "include",
          model: "Comment",
          argsPath: "args.select.posts.include.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "select",
            model: "Post",
            argsPath: "args.select.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "nested select in nested include",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        include: {
          posts: {
            include: {
              comments: {
                select: {
                  content: true,
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "include",
          model: "Comment",
          argsPath: "args.include.posts.include.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.include.posts.include.comments.select",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "include",
            model: "Comment",
            argsPath: "args.include.posts.include.comments",
            relations: {
              to: getModelRelation("Post", "comments"),
              from: getModelRelation("Comment", "post"),
            },
            scope: {
              operation: "include",
              model: "Post",
              argsPath: "args.include.posts",
              relations: {
                to: getModelRelation("User", "posts"),
                from: getModelRelation("Post", "author"),
              },
            },
          },
        },
      ],
    },
    {
      description: "nested include in nested select",
      rootParams: createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        select: {
          posts: {
            select: {
              comments: {
                include: {
                  author: true,
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.select.posts.select.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "select",
            model: "Post",
            argsPath: "args.select.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "include",
          model: "User",
          argsPath: "args.select.posts.select.comments.include.author",
          relations: {
            to: getModelRelation("Comment", "author"),
            from: getModelRelation("User", "comments"),
          },
          scope: {
            operation: "select",
            model: "Comment",
            argsPath: "args.select.posts.select.comments",
            relations: {
              to: getModelRelation("Post", "comments"),
              from: getModelRelation("Comment", "post"),
            },
            scope: {
              operation: "select",
              model: "Post",
              argsPath: "args.select.posts",
              relations: {
                to: getModelRelation("User", "posts"),
                from: getModelRelation("Post", "author"),
              },
            },
          },
        },
      ],
    },
    {
      description: "nested includes with nested create",
      rootParams: createParams(query, "User", "create", {
        data: {
          email: faker.internet.email(),
          posts: {
            create: {
              title: faker.lorem.sentence(),
              comments: {
                create: {
                  authorId: faker.datatype.number(),
                  content: faker.lorem.sentence(),
                },
              },
            },
          },
        },
        include: {
          posts: {
            include: {
              comments: true,
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Post",
          argsPath: "args.data.posts.create",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "create",
          model: "Comment",
          argsPath: "args.data.posts.create.comments.create",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "create",
            model: "Post",
            argsPath: "args.data.posts.create",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "include",
          model: "Comment",
          argsPath: "args.include.posts.include.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "nested selects with nested create",
      rootParams: createParams(query, "User", "create", {
        data: {
          email: faker.internet.email(),
          posts: {
            create: {
              title: faker.lorem.sentence(),
              comments: {
                create: {
                  authorId: faker.datatype.number(),
                  content: faker.lorem.sentence(),
                },
              },
            },
          },
        },
        select: {
          posts: {
            select: {
              comments: true,
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "create",
          model: "Post",
          argsPath: "args.data.posts.create",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "create",
          model: "Comment",
          argsPath: "args.data.posts.create.comments.create",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "create",
            model: "Post",
            argsPath: "args.data.posts.create",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.select.posts.select.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "select",
            model: "Post",
            argsPath: "args.select.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "nested where",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          profile: {
            bio: {
              contains: "foo",
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Profile",
          argsPath: "args.where.profile",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested where with list modifier",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          posts: {
            some: {
              title: {
                contains: "foo",
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.posts.some",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          modifier: "some",
        },
      ],
    },
    {
      description: "nested where with multiple list modifiers",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          posts: {
            none: {
              title: "foo",
            },
            every: {
              title: "bar",
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.posts.none",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          modifier: "none",
        },
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.posts.every",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          modifier: "every",
        },
      ],
    },
    {
      description: "nested where with 'is' modifier",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          profile: {
            is: {
              bio: {
                contains: "foo",
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Profile",
          argsPath: "args.where.profile.is",
          modifier: "is",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "nested where with 'isNot' modifier",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          profile: {
            isNot: {
              bio: {
                contains: "foo",
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Profile",
          argsPath: "args.where.profile.isNot",
          modifier: "isNot",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "two nested where calls from two relations on same parent",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          profile: {
            bio: {
              contains: "foo",
            },
          },
          posts: {
            some: {
              title: {
                contains: "foo",
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Profile",
          argsPath: "args.where.profile",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.posts.some",
          modifier: "some",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "deeply nested where",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          profile: {
            user: { name: "foo" },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Profile",
          argsPath: "args.where.profile",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
        {
          operation: "where",
          model: "User",
          argsPath: "args.where.profile.user",
          relations: {
            to: getModelRelation("Profile", "user"),
            from: getModelRelation("User", "profile"),
          },
          scope: {
            operation: "where",
            model: "Profile",
            argsPath: "args.where.profile",
            relations: {
              to: getModelRelation("User", "profile"),
              from: getModelRelation("Profile", "user"),
            },
          },
        },
      ],
    },
    {
      description: "where nested in 'some' modifier",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          posts: {
            some: {
              author: {
                name: "foo",
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.posts.some",
          modifier: "some",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "User",
          argsPath: "args.where.posts.some.author",
          relations: {
            to: getModelRelation("Post", "author"),
            from: getModelRelation("User", "posts"),
          },
          scope: {
            operation: "where",
            model: "Post",
            argsPath: "args.where.posts.some",
            modifier: "some",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "where nested in 'none' modifier",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          posts: {
            none: {
              author: {
                name: "foo",
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.posts.none",
          modifier: "none",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "User",
          argsPath: "args.where.posts.none.author",
          relations: {
            to: getModelRelation("Post", "author"),
            from: getModelRelation("User", "posts"),
          },
          scope: {
            operation: "where",
            model: "Post",
            argsPath: "args.where.posts.none",
            modifier: "none",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "where nested in 'every' modifier",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          posts: {
            every: {
              author: {
                name: "foo",
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.posts.every",
          modifier: "every",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "User",
          argsPath: "args.where.posts.every.author",
          relations: {
            to: getModelRelation("Post", "author"),
            from: getModelRelation("User", "posts"),
          },
          scope: {
            operation: "where",
            model: "Post",
            argsPath: "args.where.posts.every",
            modifier: "every",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "where nested in 'is' modifier",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          profile: {
            is: {
              user: {
                name: "foo",
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Profile",
          argsPath: "args.where.profile.is",
          modifier: "is",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
        {
          operation: "where",
          model: "User",
          argsPath: "args.where.profile.is.user",
          relations: {
            to: getModelRelation("Profile", "user"),
            from: getModelRelation("User", "profile"),
          },
          scope: {
            operation: "where",
            model: "Profile",
            argsPath: "args.where.profile.is",
            modifier: "is",
            relations: {
              to: getModelRelation("User", "profile"),
              from: getModelRelation("Profile", "user"),
            },
          },
        },
      ],
    },
    {
      description: "where nested in 'isNot' modifier",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          profile: {
            isNot: {
              user: {
                name: "foo",
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Profile",
          argsPath: "args.where.profile.isNot",
          modifier: "isNot",
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
        {
          operation: "where",
          model: "User",
          argsPath: "args.where.profile.isNot.user",
          relations: {
            to: getModelRelation("Profile", "user"),
            from: getModelRelation("User", "profile"),
          },
          scope: {
            operation: "where",
            model: "Profile",
            argsPath: "args.where.profile.isNot",
            modifier: "isNot",
            relations: {
              to: getModelRelation("User", "profile"),
              from: getModelRelation("Profile", "user"),
            },
          },
        },
      ],
    },
    {
      description: "where nested in AND logical operator",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          AND: [
            {
              posts: {
                some: {
                  content: "foo",
                },
              },
            },
            {
              comments: {
                some: {
                  content: "bar",
                },
              },
            },
          ],
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.AND.0.posts.some",
          modifier: "some",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          logicalOperators: ["AND"],
        },
        {
          operation: "where",
          model: "Comment",
          argsPath: "args.where.AND.1.comments.some",
          modifier: "some",
          relations: {
            to: getModelRelation("User", "comments"),
            from: getModelRelation("Comment", "author"),
          },
          logicalOperators: ["AND"],
        },
      ],
    },
    {
      description: "where nested in OR logical operator",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          OR: [
            {
              posts: {
                some: {
                  content: "foo",
                },
              },
            },
            {
              comments: {
                some: {
                  content: "bar",
                },
              },
            },
          ],
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.OR.0.posts.some",
          modifier: "some",
          logicalOperators: ["OR"],
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "Comment",
          argsPath: "args.where.OR.1.comments.some",
          modifier: "some",
          logicalOperators: ["OR"],
          relations: {
            to: getModelRelation("User", "comments"),
            from: getModelRelation("Comment", "author"),
          },
        },
      ],
    },
    {
      description: "where nested in NOT logical operator",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          NOT: [
            {
              posts: {
                some: {
                  content: "foo",
                },
              },
            },
            {
              comments: {
                some: {
                  content: "bar",
                },
              },
            },
          ],
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.NOT.0.posts.some",
          modifier: "some",
          logicalOperators: ["NOT"],
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "Comment",
          argsPath: "args.where.NOT.1.comments.some",
          modifier: "some",
          logicalOperators: ["NOT"],
          relations: {
            to: getModelRelation("User", "comments"),
            from: getModelRelation("Comment", "author"),
          },
        },
      ],
    },
    {
      description: "where nested in NOT, AND and OR logical operator",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          NOT: [
            {
              posts: {
                some: {
                  content: "foo",
                },
              },
            },
          ],
          AND: [
            {
              comments: {
                some: {
                  content: "bar",
                },
              },
            },
          ],
          OR: [
            {
              profile: {
                bio: "baz",
              },
            },
          ],
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.NOT.0.posts.some",
          modifier: "some",
          logicalOperators: ["NOT"],
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "Comment",
          argsPath: "args.where.AND.0.comments.some",
          modifier: "some",
          logicalOperators: ["AND"],
          relations: {
            to: getModelRelation("User", "comments"),
            from: getModelRelation("Comment", "author"),
          },
        },
        {
          operation: "where",
          model: "Profile",
          argsPath: "args.where.OR.0.profile",
          logicalOperators: ["OR"],
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
      ],
    },
    {
      description: "where deeply nested in logical modifiers",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          NOT: [
            {
              posts: {
                some: {
                  AND: [{ author: { OR: [{ name: "foo" }, { name: "bar" }] } }],
                },
              },
            },
          ],
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.NOT.0.posts.some",
          modifier: "some",
          logicalOperators: ["NOT"],
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "User",
          argsPath: "args.where.NOT.0.posts.some.AND.0.author",
          logicalOperators: ["AND"],
          relations: {
            to: getModelRelation("Post", "author"),
            from: getModelRelation("User", "posts"),
          },
          scope: {
            operation: "where",
            model: "Post",
            argsPath: "args.where.NOT.0.posts.some",
            modifier: "some",
            logicalOperators: ["NOT"],
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description:
        "where deeply nested in logical operators with no interim relations",
      rootParams: createParams(query, "User", "findMany", {
        where: {
          NOT: {
            AND: [
              {
                NOT: {
                  name: "foo",
                  profile: {
                    bio: "bar",
                  },
                },
              },
              {
                OR: [
                  {
                    name: "foo",
                  },
                  {
                    name: "bar",
                    comments: {
                      some: {
                        content: "baz",
                      },
                    },
                  },
                ],
              },
            ],
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Profile",
          argsPath: "args.where.NOT.AND.0.NOT.profile",
          logicalOperators: ["NOT", "AND", "NOT"],
          relations: {
            to: getModelRelation("User", "profile"),
            from: getModelRelation("Profile", "user"),
          },
        },
        {
          operation: "where",
          model: "Comment",
          argsPath: "args.where.NOT.AND.1.OR.1.comments.some",
          modifier: "some",
          logicalOperators: ["NOT", "AND", "OR"],
          relations: {
            to: getModelRelation("User", "comments"),
            from: getModelRelation("Comment", "author"),
          },
        },
      ],
    },
    {
      description: "include where",
      rootParams: createParams(query, "User", "findMany", {
        include: {
          posts: {
            where: {
              title: "foo",
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "Post",
          argsPath: "args.include.posts.where",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "select where",
      rootParams: createParams(query, "User", "findMany", {
        select: {
          posts: {
            where: {
              title: "foo",
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "Post",
          argsPath: "args.select.posts.where",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          scope: {
            operation: "select",
            model: "Post",
            argsPath: "args.select.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
      ],
    },
    {
      description: "select in include where",
      rootParams: createParams(query, "User", "findMany", {
        include: {
          posts: {
            select: {
              comments: {
                where: {
                  content: "foo",
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "select",
          model: "Post",
          argsPath: "args.include.posts.select",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.include.posts.select.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "where",
          model: "Comment",
          argsPath: "args.include.posts.select.comments.where",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "select",
            model: "Comment",
            argsPath: "args.include.posts.select.comments",
            relations: {
              to: getModelRelation("Post", "comments"),
              from: getModelRelation("Comment", "post"),
            },
            scope: {
              operation: "include",
              model: "Post",
              argsPath: "args.include.posts",
              relations: {
                to: getModelRelation("User", "posts"),
                from: getModelRelation("Post", "author"),
              },
            },
          },
        },
      ],
    },
    {
      description: "multiple include wheres",
      rootParams: createParams(query, "User", "findMany", {
        include: {
          posts: {
            where: {
              title: "foo",
            },
            include: {
              comments: {
                where: {
                  content: "foo",
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "Post",
          argsPath: "args.include.posts.where",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "include",
          model: "Comment",
          argsPath: "args.include.posts.include.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "where",
          model: "Comment",
          argsPath: "args.include.posts.include.comments.where",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "include",
            model: "Comment",
            argsPath: "args.include.posts.include.comments",
            relations: {
              to: getModelRelation("Post", "comments"),
              from: getModelRelation("Comment", "post"),
            },
            scope: {
              operation: "include",
              model: "Post",
              argsPath: "args.include.posts",
              relations: {
                to: getModelRelation("User", "posts"),
                from: getModelRelation("Post", "author"),
              },
            },
          },
        },
      ],
    },
    {
      description: "multiple select wheres",
      rootParams: createParams(query, "User", "findMany", {
        select: {
          posts: {
            where: {
              title: "foo",
            },
            select: {
              comments: {
                where: {
                  content: "foo",
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "select",
          model: "Post",
          argsPath: "args.select.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "Post",
          argsPath: "args.select.posts.where",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          scope: {
            operation: "select",
            model: "Post",
            argsPath: "args.select.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "select",
          model: "Comment",
          argsPath: "args.select.posts.select.comments",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "select",
            model: "Post",
            argsPath: "args.select.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "where",
          model: "Comment",
          argsPath: "args.select.posts.select.comments.where",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          scope: {
            operation: "select",
            model: "Comment",
            argsPath: "args.select.posts.select.comments",
            relations: {
              to: getModelRelation("Post", "comments"),
              from: getModelRelation("Comment", "post"),
            },
            scope: {
              operation: "select",
              model: "Post",
              argsPath: "args.select.posts",
              relations: {
                to: getModelRelation("User", "posts"),
                from: getModelRelation("Post", "author"),
              },
            },
          },
        },
      ],
    },
    {
      description: "include where with nested relations",
      rootParams: createParams(query, "User", "findMany", {
        include: {
          posts: {
            where: {
              title: "foo",
              comments: {
                some: {
                  content: "bar",
                  repliedTo: {
                    is: {
                      content: "baz",
                      author: {
                        id: 1,
                      },
                    },
                  },
                },
              },
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Post",
          argsPath: "args.include.posts",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
        {
          operation: "where",
          model: "Post",
          argsPath: "args.include.posts.where",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
          scope: {
            operation: "include",
            model: "Post",
            argsPath: "args.include.posts",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
          },
        },
        {
          operation: "where",
          model: "Comment",
          argsPath: "args.include.posts.where.comments.some",
          relations: {
            to: getModelRelation("Post", "comments"),
            from: getModelRelation("Comment", "post"),
          },
          modifier: "some",
          scope: {
            operation: "where",
            model: "Post",
            argsPath: "args.include.posts.where",
            relations: {
              to: getModelRelation("User", "posts"),
              from: getModelRelation("Post", "author"),
            },
            scope: {
              operation: "include",
              model: "Post",
              argsPath: "args.include.posts",
              relations: {
                to: getModelRelation("User", "posts"),
                from: getModelRelation("Post", "author"),
              },
            },
          },
        },
        {
          operation: "where",
          model: "Comment",
          argsPath: "args.include.posts.where.comments.some.repliedTo.is",
          relations: {
            to: getModelRelation("Comment", "repliedTo"),
            from: getModelRelation("Comment", "replies"),
          },
          modifier: "is",
          scope: {
            operation: "where",
            model: "Comment",
            argsPath: "args.include.posts.where.comments.some",
            relations: {
              to: getModelRelation("Post", "comments"),
              from: getModelRelation("Comment", "post"),
            },
            modifier: "some",
            scope: {
              operation: "where",
              model: "Post",
              argsPath: "args.include.posts.where",
              relations: {
                to: getModelRelation("User", "posts"),
                from: getModelRelation("Post", "author"),
              },
              scope: {
                operation: "include",
                model: "Post",
                argsPath: "args.include.posts",
                relations: {
                  to: getModelRelation("User", "posts"),
                  from: getModelRelation("Post", "author"),
                },
              },
            },
          },
        },
        {
          operation: "where",
          model: "User",
          argsPath:
            "args.include.posts.where.comments.some.repliedTo.is.author",
          relations: {
            to: getModelRelation("Comment", "author"),
            from: getModelRelation("User", "comments"),
          },
          scope: {
            operation: "where",
            model: "Comment",
            argsPath: "args.include.posts.where.comments.some.repliedTo.is",
            relations: {
              to: getModelRelation("Comment", "repliedTo"),
              from: getModelRelation("Comment", "replies"),
            },
            modifier: "is",
            scope: {
              operation: "where",
              model: "Comment",
              argsPath: "args.include.posts.where.comments.some",
              relations: {
                to: getModelRelation("Post", "comments"),
                from: getModelRelation("Comment", "post"),
              },
              modifier: "some",
              scope: {
                operation: "where",
                model: "Post",
                argsPath: "args.include.posts.where",
                relations: {
                  to: getModelRelation("User", "posts"),
                  from: getModelRelation("Post", "author"),
                },
                scope: {
                  operation: "include",
                  model: "Post",
                  argsPath: "args.include.posts",
                  relations: {
                    to: getModelRelation("User", "posts"),
                    from: getModelRelation("Post", "author"),
                  },
                },
              },
            },
          },
        },
      ],
    },
    {
      description: "where in groupBy",
      rootParams: createParams(query, "User", "groupBy", {
        by: ["id"],
        orderBy: { id: "asc" },
        where: {
          posts: {
            some: {
              title: "foo",
            },
          },
        },
      }),
      nestedCalls: [
        {
          operation: "where",
          model: "Post",
          argsPath: "args.where.posts.some",
          modifier: "some",
          relations: {
            to: getModelRelation("User", "posts"),
            from: getModelRelation("Post", "author"),
          },
        },
      ],
    },
    {
      description: "correct relations for relations between same model",
      rootParams: createParams(query, "Comment", "findMany", {
        include: {
          replies: true,
        },
      }),
      nestedCalls: [
        {
          operation: "include",
          model: "Comment",
          argsPath: "args.include.replies",
          relations: {
            to: getModelRelation("Comment", "replies"),
            from: getModelRelation("Comment", "repliedTo"),
          },
        },
      ],
    },
  ])(
    "calls middleware with $description",
    async ({ rootParams, nestedCalls = [] }) => {
      const $rootOperation = jest.fn((params) => params.query(params.args));
      const $allNestedOperations = jest.fn((params) => params.query(params.args));
      const allOperations = withNestedOperations({
        $rootOperation,
        $allNestedOperations,
      });

      await allOperations(rootParams as any);

      expect($rootOperation).toHaveBeenCalledTimes(1);
      expect($rootOperation).toHaveBeenCalledWith({
        ...rootParams,
        query: expect.any(Function),
      });
      expect($allNestedOperations).toHaveBeenCalledTimes(nestedCalls.length);
      nestedCalls.forEach((call) => {
        expect($allNestedOperations).toHaveBeenCalledWith({
          ...nestedParamsFromCall(rootParams, call),
          query: expect.any(Function),
        });
      });
    }
  );
});
