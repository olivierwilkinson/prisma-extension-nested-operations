import { Post, Prisma, User } from "@prisma/client";
import faker from "faker";
import { set } from "lodash";

import { withNestedOperations } from "../../src";
import client from "./client";

describe("smoke", () => {
  let testClient: any;
  let firstUser: User;
  let secondUser: User;
  let post: Post;

  beforeEach(async () => {
    firstUser = await client.user.create({
      data: {
        email: faker.internet.email(),
        name: "Jack",
      },
    });
    secondUser = await client.user.create({
      data: {
        email: faker.internet.email(),
        name: "John",
      },
    });
    await client.profile.create({
      data: {
        bio: faker.lorem.sentence(),
        user: {
          connect: {
            id: firstUser.id,
          },
        },
      },
    });
    post = await client.post.create({
      data: {
        title: faker.lorem.sentence(),
        authorId: firstUser.id,
      },
    });
    await client.comment.create({
      data: {
        content: faker.lorem.sentence(),
        author: {
          connect: {
            id: firstUser.id,
          },
        },
        post: {
          connect: {
            id: post.id,
          },
        },
      },
    });
  });
  afterEach(async () => {
    await client.comment.deleteMany({ where: {} });
    await client.post.deleteMany({ where: {} });
    await client.profile.deleteMany({ where: {} });
    await client.user.deleteMany({ where: {} });
  });
  afterAll(async () => {
    await testClient.$disconnect();
  });

  describe("vanilla", () => {
    beforeAll(() => {
      testClient = client.$extends({
        query: {
          $allModels: {
            $allOperations: withNestedOperations({
              $rootOperation: ({ args, query }) => {
                return query(args);
              },
              $allNestedOperations: ({ args, query }) => {
                return query(args);
              },
            }),
          },
        },
      });
    });

    it("does not break client when middleware does nothing", async () => {
      const user = await testClient.user.findFirst({
        where: { id: firstUser.id },
      });
      expect(user).not.toBeNull();

      const users = await testClient.user.findMany({
        where: { id: { in: [firstUser.id, secondUser.id] } },
      });
      expect(users).toHaveLength(2);

      const dbProfile = await testClient.profile.findFirst({
        where: { user: { id: firstUser.id } },
      });
      expect(dbProfile).not.toBeNull();

      const dbComment = await testClient.comment.findFirst({
        where: { post: { author: { id: firstUser.id } } },
      });
      expect(dbComment).not.toBeNull();

      const dbComments = await testClient.comment.findMany({
        where: { post: { author: { id: firstUser.id } } },
        include: {
          post: {
            include: {
              comments: {
                select: {
                  id: true,
                },
              },
            },
          },
        },
      });
      expect(dbComments).toHaveLength(1);
      expect(dbComments[0].post).not.toBeNull();
      expect(dbComments[0].post!.comments).toHaveLength(1);

      const createdComment = await testClient.comment.create({
        data: {
          content: faker.lorem.sentence(),
          authorId: firstUser.id,
        },
      });
      expect(createdComment).not.toBeNull();

      const updatedComment = await testClient.comment.update({
        where: { id: createdComment.id },
        data: {
          content: faker.lorem.sentence(),
        },
      });
      expect(updatedComment).not.toBeNull();

      // supports Json null values
      await testClient.profile.create({
        data: {
          userId: secondUser.id,
          meta: Prisma.DbNull,
        },
      });

      const profile = await testClient.profile.findFirst({
        where: {
          OR: [
            { meta: { equals: Prisma.AnyNull } },
            { meta: { equals: Prisma.DbNull } },
            { meta: { equals: Prisma.JsonNull } },
          ],
        },
      });
      expect(profile).not.toBeNull();
      expect(profile!.meta).toBe(null);
    });
  });

  describe("groupBy", () => {
    beforeAll(() => {
      testClient = client.$extends({
        query: {
          $allModels: {
            $allOperations: withNestedOperations({
              $rootOperation: async ({ operation, args, query }) => {
                if (operation !== "groupBy") {
                  await Promise.resolve();
                  throw new Error("expected groupBy action");
                }
                return query(args);
              },
              $allNestedOperations: ({ args, query }) => {
                return query(args);
              },
            }),
          },
        },
      });
    });

    it("calls middleware with groupBy action", async () => {
      await expect(testClient.comment.findMany()).rejects.toThrowError(
        "expected groupBy action"
      );

      const groupBy = await testClient.comment.groupBy({
        by: ["authorId"],
        orderBy: {
          authorId: "asc",
        },
      });

      expect(groupBy).toHaveLength(1);
    });
  });

  describe("Changing Operations", () => {
    beforeAll(() => {
      testClient = client.$extends({
        query: {
          $allModels: {
            $allOperations: withNestedOperations({
              $rootOperation: ({ model, operation, args, query }) => {
                if (model === "User" && operation === "updateMany") {
                  return client.user.deleteMany({
                    where: { id: args.where?.id },
                  });
                }

                return query(args);
              },
              $allNestedOperations: ({ model, operation, args, query }) => {
                if (model === "Profile" && operation === "update") {
                  return query(true, "delete");
                }
                return query(args);
              },
            }),
          },
        },
      });
    });

    it("changes root operation", async () => {
      await testClient.user.updateMany({
        where: { id: firstUser.id },
        data: {
          email: faker.internet.email(),
        },
      });
      const dbUser = await testClient.user.findUnique({
        where: { id: firstUser.id },
      });
      expect(dbUser).toBeNull();
    });

    it("changes nested operation", async () => {
      await testClient.user.update({
        where: { id: firstUser.id },
        data: {
          profile: {
            update: {
              bio: faker.lorem.sentence(),
            },
          },
        },
      });
      const dbUser = await testClient.user.findUnique({
        where: { id: firstUser.id },
        include: {
          profile: true,
        },
      });
      expect(dbUser.profile).toBeNull();
    });
  });
});
