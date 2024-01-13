import { Prisma } from "@prisma/client";
import faker from "faker";
import { set } from "lodash";

import { withNestedOperations } from "../../src";
import { createParams } from "./helpers/createParams";
import { wait } from "./helpers/wait";

describe("args", () => {
  it("does not mutate passed params object", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        // @ts-expect-error - testing for mutation
        params.args.test = "test";
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        params.args.test = "test";
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        posts: {
          create: { title: faker.lorem.sentence() },
        },
      },
    });
    await allOperations(params);

    expect(params.args).not.toHaveProperty("test");
    expect(params.args.data.posts.create).not.toHaveProperty("test");
  });

  it("passes through instances of Prisma.NullTypes to query", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "Profile", "updateMany", {
      where: {
        OR: [
          { meta: { equals: Prisma.JsonNull } },
          { meta: { equals: Prisma.DbNull } },
          { meta: { equals: Prisma.AnyNull } },
        ],
      },
      data: [
        { meta: Prisma.JsonNull },
        { meta: Prisma.DbNull },
        { meta: Prisma.AnyNull },
      ],
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith(params.args);
    expect(query.mock.calls[0][0].where.OR).toHaveLength(3);
    query.mock.calls[0][0].where.OR.forEach(({ meta }: any, index: number) => {
      expect(meta.equals).toBe(params.args.where.OR[index].meta.equals);
    });
    query.mock.calls[0][0].data.forEach(({ meta }: any, index: number) => {
      expect(meta).toBe(params.args.data[index].meta);
    });
  });

  it("can modify root args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        if (params.operation === "create" && params.model === "User") {
          return params.query({
            ...params.args,
            data: set(params.args.data, "name", "Default Name"),
          });
        }
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve({}));
    const params = createParams(query, "User", "create", {
      data: { email: faker.internet.email() },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        name: "Default Name",
      },
    });
  });

  it("can modify root args asynchronously", async () => {
    const allOperations = withNestedOperations({
      async $rootOperation(params) {
        if (params.operation === "create" && params.model === "User") {
          await wait(100);
          return params.query({
            ...params.args,
            data: set(params.args.data, "name", "Default Name"),
          });
        }
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: { email: faker.internet.email() },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        name: "Default Name",
      },
    });
  });

  it("can modify nested args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.model === "Post") {
          return params.query({
            ...params.args,
            number: faker.datatype.number(),
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        posts: {
          create: { title: faker.lorem.sentence() },
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        posts: {
          create: {
            title: params.args.data.posts.create.title,
            number: expect.any(Number),
          },
        },
      },
    });
  });

  it("can modify nested args asynchronously", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: async (params) => {
        if (params.model === "Post") {
          await wait(100);
          return params.query({
            ...params.args,
            number: faker.datatype.number(),
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        posts: {
          create: { title: faker.lorem.sentence() },
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        posts: {
          create: {
            title: params.args.data.posts.create.title,
            number: expect.any(Number),
          },
        },
      },
    });
  });

  it("can modify nested create list args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.model === "Post") {
          return params.query({
            ...params.args,
            number: params.args.title === "first" ? 1 : 2,
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        posts: {
          create: [{ title: "first" }, { title: "second" }],
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        posts: {
          create: [
            { title: "first", number: 1 },
            { title: "second", number: 2 },
          ],
        },
      },
    });
  });

  it("can modify deeply nested toOne update args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.model === "Comment") {
          if (params.scope && !params.scope.relations.to.isList) {
            return params.query({
              ...params.args,
              number: parseInt(params.args.content, 10),
            });
          }

          return params.query({
            ...params.args,
            data: {
              ...params.args.data,
              number: parseInt(params.args.data.content, 10),
            },
          });
        }

        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "update", {
      where: { id: faker.datatype.number() },
      data: {
        email: faker.internet.email(),
        comments: {
          update: {
            where: { id: faker.datatype.number() },
            data: {
              content: "1",
              repliedTo: {
                update: {
                  content: "2",
                  repliedTo: {
                    update: {
                      content: "3",
                    },
                  },
                },
              },
            },
          },
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        comments: {
          update: {
            where: params.args.data.comments.update.where,
            data: {
              content: "1",
              number: 1,
              repliedTo: {
                update: {
                  content: "2",
                  number: 2,
                  repliedTo: {
                    update: {
                      content: "3",
                      number: 3,
                    },
                  },
                },
              },
            },
          },
        },
      },
    });
  });

  it("can modify nested update list args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.model === "Post") {
          return params.query({
            ...params.args,
            data: {
              ...params.args.data,
              number: params.args.data.title === "first" ? 1 : 2,
            },
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "update", {
      where: { id: faker.datatype.number() },
      data: {
        email: faker.internet.email(),
        posts: {
          update: [
            {
              where: { id: faker.datatype.number() },
              data: { title: "first" },
            },
            {
              where: { id: faker.datatype.number() },
              data: { title: "second" },
            },
          ],
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        posts: {
          update: [
            {
              where: params.args.data.posts.update[0].where,
              data: { title: "first", number: 1 },
            },
            {
              where: params.args.data.posts.update[1].where,
              data: { title: "second", number: 2 },
            },
          ],
        },
      },
    });
  });

  it("can modify nested delete list args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "delete" && params.model === "Post") {
          return params.query({ id: params.args.id + 1 });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "update", {
      where: { id: faker.datatype.number() },
      data: {
        email: faker.internet.email(),
        posts: {
          delete: [{ id: 1 }, { id: 2 }],
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        posts: {
          delete: [{ id: 2 }, { id: 3 }],
        },
      },
    });
  });

  it("can modify args of operations nested in list", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "create" && params.model === "Comment") {
          return params.query({
            ...params.args,
            number: params.args.content === "first post comment" ? 1 : 2,
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "update", {
      where: { id: faker.datatype.number() },
      data: {
        email: faker.internet.email(),
        posts: {
          update: [
            {
              where: { id: faker.datatype.number() },
              data: {
                title: "first",
                comments: {
                  create: {
                    content: "first post comment",
                    authorId: faker.datatype.number(),
                  },
                },
              },
            },
            {
              where: { id: faker.datatype.number() },
              data: {
                title: "second",
                comments: {
                  create: {
                    content: "second post comment",
                    authorId: faker.datatype.number(),
                  },
                },
              },
            },
          ],
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        posts: {
          update: [
            {
              where: params.args.data.posts.update[0].where,
              data: {
                title: "first",
                comments: {
                  create: {
                    content: "first post comment",
                    authorId: expect.any(Number),
                    number: 1,
                  },
                },
              },
            },
            {
              where: params.args.data.posts.update[1].where,
              data: {
                title: "second",
                comments: {
                  create: {
                    content: "second post comment",
                    authorId: expect.any(Number),
                    number: 2,
                  },
                },
              },
            },
          ],
        },
      },
    });
  });

  it("can modify args of deeply nested lists of create operations", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "create" && params.model === "Comment") {
          if (params.scope) {
            return params.query({
              ...params.args,
              number: params.args.content === "first post comment" ? 1 : 2,
            });
          }

          return params.query({
            ...params.args,
            data: {
              ...params.args.data,
              number: params.args.data.content === "first post comment" ? 1 : 2,
            },
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "update", {
      where: { id: faker.datatype.number() },
      data: {
        email: faker.internet.email(),
        posts: {
          create: [
            {
              title: "first",
              comments: {
                create: [
                  {
                    content: "first post comment",
                    authorId: faker.datatype.number(),
                  },
                  {
                    content: "second post comment",
                    authorId: faker.datatype.number(),
                  },
                ],
              },
            },
            {
              title: "second",
              comments: {
                create: [
                  {
                    content: "first post comment",
                    authorId: faker.datatype.number(),
                  },
                  {
                    content: "second post comment",
                    authorId: faker.datatype.number(),
                  },
                ],
              },
            },
          ],
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        posts: {
          create: [
            {
              title: "first",
              comments: {
                create: [
                  {
                    content: "first post comment",
                    authorId: expect.any(Number),
                    number: 1,
                  },
                  {
                    content: "second post comment",
                    authorId: expect.any(Number),
                    number: 2,
                  },
                ],
              },
            },
            {
              title: "second",
              comments: {
                create: [
                  {
                    content: "first post comment",
                    authorId: expect.any(Number),
                    number: 1,
                  },
                  {
                    content: "second post comment",
                    authorId: expect.any(Number),
                    number: 2,
                  },
                ],
              },
            },
          ],
        },
      },
    });
  });

  it("can modify include args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        if (params.operation === "create" && params.model === "User") {
          return params.query({
            ...params.args,
            include: {
              posts: params.args.include?.posts && {
                include: {
                  comments: true,
                },
              },
            },
          });
        }
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
      },
      include: {
        posts: true,
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      include: {
        posts: {
          include: {
            comments: true,
          },
        },
      },
    });
  });

  it("can modify include args through include actions", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "include" && params.model === "Post") {
          return params.query({
            orderBy: { createdAt: "desc" },
            comments: true,
            skip: params.args.skip + 1,
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
      },
      include: {
        posts: {
          orderBy: { createdAt: "asc" },
          skip: 10,
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      include: {
        ...params.args.include,
        posts: {
          ...params.args.include.posts,
          orderBy: { createdAt: "desc" },
          comments: true,
          skip: 11,
        },
      },
    });
  });

  it("can modify deeply nested include args through include action", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "include" && params.model === "Comment") {
          if (params.args.skip) {
            params.args.skip += 1;
          }
          return params.query({
            ...params.args,
            orderBy: { createdAt: "desc" },
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
      },
      include: {
        posts: {
          include: {
            comments: {
              include: { replies: { skip: 10 } },
            },
          },
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      include: {
        posts: {
          include: {
            comments: {
              orderBy: { createdAt: "desc" },
              include: {
                replies: {
                  orderBy: { createdAt: "desc" },
                  skip: 11,
                },
              },
            },
          },
        },
      },
    });
  });

  it("can modify select args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        if (params.operation === "create" && params.model === "User") {
          return params.query({
            ...params.args,
            select: {
              email: true,
              posts: params.args.select?.posts && {
                select: {
                  title: true,
                },
              },
            },
          });
        }
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
      },
      select: { posts: true },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      select: {
        email: true,
        posts: {
          select: {
            title: true,
          },
        },
      },
    });
  });

  it("can modify select args through select action", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "select" && params.model === "Post") {
          return params.query({
            ...params.args,
            select: {
              title: true,
              comments: params.args.select.comments && {
                select: {
                  content: true,
                },
              },
            },
          });
        }
        if (params.operation === "select" && params.model === "Comment") {
          return params.query({
            ...params.args,
            select: {
              content: true,
            },
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
      },
      select: {
        posts: {
          select: {
            comments: true,
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      select: {
        posts: {
          select: {
            title: true,
            comments: {
              select: {
                content: true,
              },
            },
          },
        },
      },
    });
  });

  it("can modify deeply nested select args through select action", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "select" && params.model === "Comment") {
          return params.query({
            ...params.args,
            select: {
              ...(typeof params.args.select === "boolean"
                ? {}
                : params.args.select),
              content: true,
            },
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
      },
      select: {
        posts: {
          select: {
            comments: {
              select: { replies: true },
            },
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      select: {
        posts: {
          select: {
            comments: {
              select: {
                content: true,
                replies: {
                  select: {
                    content: true,
                  },
                },
              },
            },
          },
        },
      },
    });
  });

  it("can modify select args nested in include select", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => params.query(params.args),
      $allNestedOperations: (params) => {
        if (params.operation === "select" && params.model === "Comment") {
          return params.query({
            where: { deleted: true },
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
      },
      include: {
        posts: {
          select: {
            comments: true,
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      include: {
        posts: {
          select: {
            comments: {
              where: {
                deleted: true,
              },
            },
          },
        },
      },
    });
  });

  it("can add data to nested createMany args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "createMany") {
          return params.query({
            ...params.args,
            data: [
              ...params.args.data.map((data: any) => ({
                ...data,
                number: faker.datatype.number(),
              })),
              {
                content: faker.lorem.sentence(),
                number: faker.datatype.number(),
              },
            ],
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        comments: {
          createMany: { data: [{ content: faker.lorem.sentence() }] },
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        comments: {
          createMany: {
            data: [
              {
                content: params.args.data.comments.createMany.data[0].content,
                number: expect.any(Number),
              },
              { content: expect.any(String), number: expect.any(Number) },
            ],
          },
        },
      },
    });
  });

  it("allows user to reorder nested createMany args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "createMany") {
          return params.query({
            ...params.args,
            data: [...params.args.data].reverse(),
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        comments: {
          createMany: {
            data: [{ content: "first" }, { content: "second" }],
          },
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        comments: {
          createMany: {
            data: [{ content: "second" }, { content: "first" }],
          },
        },
      },
    });
  });

  it("allows user to add data to nested createMany args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "createMany") {
          return params.query({
            ...params.args,
            data: [
              ...params.args.data.map((data: any) => ({
                ...data,
                number: faker.datatype.number(),
              })),
              {
                content: faker.lorem.sentence(),
                number: faker.datatype.number(),
              },
            ],
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        comments: {
          createMany: { data: [{ content: faker.lorem.sentence() }] },
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        comments: {
          createMany: {
            data: [
              {
                content: params.args.data.comments.createMany.data[0].content,
                number: expect.any(Number),
              },
              { content: expect.any(String), number: expect.any(Number) },
            ],
          },
        },
      },
    });
  });

  it("allows user to remove data from nested createMany args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "createMany") {
          return params.query({
            ...params.args,
            data: [
              { ...params.args.data[0], number: faker.datatype.number() },
              { number: faker.datatype.number() },
            ],
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        comments: {
          createMany: {
            data: [
              { content: faker.lorem.sentence() },
              { content: faker.lorem.sentence() },
              { content: faker.lorem.sentence() },
            ],
          },
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        comments: {
          createMany: {
            data: [
              {
                content: params.args.data.comments.createMany.data[0].content,
                number: expect.any(Number),
              },
              { number: expect.any(Number) },
            ],
          },
        },
      },
    });
  });

  it("allows user to modify nested where args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "where" && params.model === "Comment") {
          return params.query({
            ...params.args,
            content: "bar",
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "findMany", {
      where: {
        posts: {
          some: {
            comments: {
              some: {
                content: "foo",
              },
            },
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "where.posts.some.comments.some.content", "bar")
    );
  });

  it("allows user to modify nested where args by removing a field", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "where" && params.model === "Comment") {
          return params.query({
            // remove content and replace it with updatedAt
            updatedAt: {
              gt: new Date(),
            },
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "findMany", {
      where: {
        posts: {
          some: {
            comments: {
              some: {
                content: "foo",
              },
            },
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "where.posts.some.comments.some", {
        updatedAt: {
          gt: expect.any(Date),
        },
      })
    );
  });

  it("allows user to modify nested where args with nested where", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "where" && params.model === "Comment") {
          return params.query({
            ...params.args,
            content: {
              contains: "bar",
            },
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "findMany", {
      where: {
        posts: {
          some: {
            comments: {
              some: {
                content: "foo",
              },
            },
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "where.posts.some.comments.some.content", {
        contains: "bar",
      })
    );
  });

  it("allows user to modify nested where args with nested where in logical operation", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "where" && params.model === "Comment") {
          return params.query({
            ...params.args,
            content: {
              contains: "bar",
            },
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "findMany", {
      where: {
        posts: {
          some: {
            AND: [
              {
                author: {
                  id: 1,
                },
              },
              {
                comments: {
                  some: {
                    content: "foo",
                  },
                },
              },
            ],
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "where.posts.some.AND.1.comments.some.content", {
        contains: "bar",
      })
    );
  });

  it("allows user to modify where args deeply nested in logical operations", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (
          params.operation === "where" &&
          params.model === "User" &&
          params.scope
        ) {
          return params.query({
            ...params.args,
            ...(params.args.id ? { id: params.args.id + 1 } : {}),
          });
        }

        if (params.operation === "where" && params.model === "Comment") {
          return params.query({
            ...params.args,
            content: "bar",
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "findMany", {
      where: {
        posts: {
          some: {
            AND: [
              {
                NOT: {
                  OR: [
                    {
                      AND: [
                        {
                          NOT: {
                            OR: [
                              {
                                id: 1,
                                author: {
                                  id: 2,
                                },
                              },
                            ],
                          },
                        },
                      ],
                      comments: {
                        some: {
                          content: "foo",
                        },
                      },
                    },
                  ],
                },
              },
            ],
          },
        },
      },
    });

    await allOperations(params);

    set(
      params,
      "args.where.posts.some.AND.0.NOT.OR.0.AND.0.NOT.OR.0.author.id",
      3
    );
    set(
      params,
      "args.where.posts.some.AND.0.NOT.OR.0.comments.some.content",
      "bar"
    );

    expect(query).toHaveBeenCalledWith(params.args);
  });

  it("allows user to modify nested include where args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "where" && params.model === "Post") {
          return params.query({
            ...params.args,
            title: "bar",
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "findUnique", {
      where: { id: 1 },
      include: {
        posts: {
          where: {
            title: "foo",
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "include.posts.where.title", "bar")
    );
  });

  it("allows user to modify nested select where args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "where" && params.model === "Post") {
          return params.query({
            ...params.args,
            title: "bar",
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "findUnique", {
      where: { id: 1 },
      select: {
        posts: {
          where: {
            title: "foo",
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "select.posts.where.title", "bar")
    );
  });

  it("allows user to modify nested where relation args in nested include where", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "where" && params.model === "Post") {
          return params.query({
            ...params.args,
            title: "bar",
          });
        }
        if (params.operation === "where" && params.model === "Comment") {
          return params.query({
            ...params.args,
            content: "bar",
          });
        }
        if (params.operation === "where" && params.model === "User") {
          return params.query({
            ...params.args,
            email: "bar",
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "findUnique", {
      where: { id: 1 },
      include: {
        posts: {
          where: {
            title: "foo",
            AND: [
              { author: { id: 1, email: "foo" } },
              { comments: { every: { content: "foo" } } },
            ],
            OR: [{ NOT: { author: { id: 1, email: "foo" } } }],
            NOT: { comments: { some: { content: "foo" } } },
          },
        },
      },
    });

    await allOperations(params);

    set(params, "args.include.posts.where.title", "bar");
    set(params, "args.include.posts.where.AND.0.author.email", "bar");
    set(params, "args.include.posts.where.AND.1.comments.every.content", "bar");
    set(params, "args.include.posts.where.OR.0.NOT.author.email", "bar");
    set(params, "args.include.posts.where.NOT.comments.some.content", "bar");

    expect(query).toHaveBeenCalledWith(params.args);
  });

  it("allows user to modify nested where relation args in nested select where", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "where" && params.model === "Post") {
          return params.query({
            ...params.args,
            title: "bar",
          });
        }
        if (params.operation === "where" && params.model === "Comment") {
          return params.query({
            ...params.args,
            content: "bar",
          });
        }
        if (params.operation === "where" && params.model === "User") {
          return params.query({
            ...params.args,
            email: "bar",
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "findUnique", {
      where: { id: 1 },
      select: {
        posts: {
          where: {
            title: "foo",
            AND: [
              { author: { id: 1, email: "foo" } },
              { comments: { every: { content: "foo" } } },
            ],
            OR: [{ NOT: { author: { id: 1, email: "foo" } } }],
            NOT: { comments: { some: { content: "foo" } } },
          },
        },
      },
    });

    await allOperations(params);

    set(params, "args.select.posts.where.title", "bar");
    set(params, "args.select.posts.where.AND.0.author.email", "bar");
    set(params, "args.select.posts.where.AND.1.comments.every.content", "bar");
    set(params, "args.select.posts.where.OR.0.NOT.author.email", "bar");
    set(params, "args.select.posts.where.NOT.comments.some.content", "bar");

    expect(query).toHaveBeenCalledWith(params.args);
  });

  it("ignores invalid values passed to where logical operations", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "where" && params.model === "Comment") {
          return params.query({
            ...params.args,
            content: {
              contains: "bar",
            },
          });
        }
        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "findMany", {
      where: {
        posts: {
          some: {
            AND: [
              {
                comments: {
                  some: {
                    content: "foo",
                  },
                },
              },
              // @ts-expect-error invalid value
              null,
              // @ts-expect-error invalid value
              undefined,
              // @ts-expect-error invalid value
              1,
              // @ts-expect-error invalid value
              "foo",
              // @ts-expect-error invalid value
              true,
            ],
            // @ts-expect-error invalid value
            NOT: null,
            // @ts-expect-error invalid value
            OR: true,
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "where.posts.some.AND.0.comments.some.content", {
        contains: "bar",
      })
    );
  });

  it("waits for all middleware to finish before calling query when modifying args", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: async (params) => {
        if (params.model === "Post") {
          await wait(100);
          return params.query({
            ...params.args,
            number: faker.datatype.number(),
          });
        }

        if (params.model === "Comment") {
          await wait(200);
          return params.query({
            ...params.args,
            number: faker.datatype.number(),
          });
        }

        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        posts: {
          create: {
            title: faker.lorem.sentence(),
            comments: {
              create: {
                content: faker.lorem.sentence(),
                authorId: faker.datatype.number(),
              },
            },
          },
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith({
      ...params.args,
      data: {
        ...params.args.data,
        posts: {
          create: {
            title: params.args.data.posts.create.title,
            number: expect.any(Number),
            comments: {
              create: {
                ...params.args.data.posts.create.comments.create,
                number: expect.any(Number),
              },
            },
          },
        },
      },
    });
  });
});
