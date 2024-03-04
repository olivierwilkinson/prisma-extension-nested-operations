import faker from "faker";
import get from "lodash/get";
import set from "lodash/set";

import { withNestedOperations } from "../../src";
import { createParams } from "./helpers/createParams";
import { wait } from "./helpers/wait";

describe("operations", () => {
  it("applies query to operations moved to new action type", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "update" && params.scope) {
          return params.query(
            {
              where: { id: params.args.where.id },
              create: params.args.data,
              update: params.args.data,
            },
            "upsert"
          );
        }

        if (params.operation === "upsert") {
          return params.query({
            ...params.args,
            update: {
              ...params.args.update,
              number: faker.datatype.number(),
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
          upsert: {
            where: { id: faker.datatype.number() },
            create: { title: faker.lorem.sentence() },
            update: { title: faker.lorem.sentence() },
          },
          update: {
            where: { id: faker.datatype.number() },
            data: { title: faker.lorem.sentence() },
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "data.posts", {
        upsert: [
          {
            where: params.args.data.posts.upsert.where,
            create: params.args.data.posts.upsert.create,
            update: {
              title: params.args.data.posts.upsert.update.title,
              number: expect.any(Number),
            },
          },
          {
            where: params.args.data.posts.update.where,
            create: params.args.data.posts.update.data,
            update: {
              title: params.args.data.posts.update.data.title,
              // this should also be applied
              number: expect.any(Number),
            },
          },
        ],
      })
    );
  });

  it("merges operation converted to existing operation correctly when converted operation defined before target operation", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "update" && params.scope) {
          return params.query(
            {
              where: { id: params.args.where.id },
              create: params.args.data,
              update: params.args.data,
            },
            "upsert"
          );
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
          update: {
            where: { id: faker.datatype.number() },
            data: { title: faker.lorem.sentence() },
          },
          upsert: {
            where: { id: faker.datatype.number() },
            create: { title: faker.lorem.sentence() },
            update: { title: faker.lorem.sentence() },
          },
        },
      },
    });

    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "data.posts", {
        upsert: [
          {
            where: params.args.data.posts.upsert.where,
            create: params.args.data.posts.upsert.create,
            update: params.args.data.posts.upsert.update,
          },
          {
            where: params.args.data.posts.update.where,
            create: params.args.data.posts.update.data,
            update: params.args.data.posts.update.data,
          },
        ],
      })
    );
  });

  it("replaces existing boolean action with new action when existing is defined before source action", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "delete") {
          return params.query(true, "disconnect");
        }

        return params.query(params.args);
      },
    });

    const query = jest.fn((_: any) => Promise.resolve(null));
    const params = createParams(query, "User", "update", {
      where: { id: faker.datatype.number() },
      data: {
        email: faker.internet.email(),
        profile: {
          delete: true,
          disconnect: false,
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "data.profile", {
        disconnect: true,
      })
    );
  });

  it("waits for all queries to finish before calling root query when modifying nested action", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: async (params) => {
        if (params.operation === "create" && params.scope) {
          await wait(100);
          return params.query(
            {
              where: { id: params.args.id },
              create: params.args,
              update: params.args,
            },
            "upsert"
          );
        }

        if (params.operation === "update") {
          await wait(200);
          return params.query(
            {
              where: { id: params.args.where.id },
              create: params.args.data,
              update: params.args.data,
            },
            "upsert"
          );
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
            id: faker.datatype.number(),
            title: faker.lorem.sentence(),
            comments: {
              create: {
                id: faker.datatype.number(),
                content: faker.lorem.sentence(),
                authorId: faker.datatype.number(),
              },
            },
          },
        },
      },
    });
    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "data.posts", {
        upsert: {
          where: { id: params.args.data.posts.create.id },
          create: {
            ...params.args.data.posts.create,
            comments: {
              upsert: {
                where: {
                  id: params.args.data.posts.create.comments.create.id,
                },
                create: params.args.data.posts.create.comments.create,
                update: params.args.data.posts.create.comments.create,
              },
            },
          },
          update: {
            ...params.args.data.posts.create,
            comments: {
              upsert: {
                where: {
                  id: params.args.data.posts.create.comments.create.id,
                },
                create: params.args.data.posts.create.comments.create,
                update: params.args.data.posts.create.comments.create,
              },
            },
          },
        },
      })
    );
  });

  it("can modify deeply nested operations", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        if (params.operation === "create") {
          if (params.scope) {
            return params.query(
              {
                where: { id: params.args.id },
                create: params.args,
                update: params.args,
              },
              "upsert"
            );
          }

          return params.query(
            {
              where: { id: params.args.id },
              create: params.args,
              update: params.args,
            },
            "upsert"
          );
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
          create: {
            id: faker.datatype.number(),
            title: faker.lorem.sentence(),
            comments: {
              create: {
                id: faker.datatype.number(),
                authorId: faker.datatype.number(),
                content: faker.lorem.sentence(),
              },
            },
          },
        },
      },
    });
    const createCommentData = params.args.data.posts.create.comments.create;
    const createPostData = {
      ...params.args.data.posts.create,
      comments: {
        upsert: {
          where: { id: createCommentData.id },
          create: createCommentData,
          update: createCommentData,
        },
      },
    };

    await allOperations(params);

    expect(query).toHaveBeenCalledWith(
      set(params.args, "data.posts", {
        upsert: {
          where: { id: createPostData.id },
          create: createPostData,
          update: createPostData,
        },
      })
    );
  });

  describe("create", () => {
    it("can modify nested create action to be an update", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "create") {
            return params.query(
              {
                where: { id: params.args.id },
                data: params.args,
              },
              "update"
            );
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
            create: {
              id: faker.datatype.number(),
              title: faker.lorem.sentence(),
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: {
            where: { id: params.args.data.posts.create.id },
            data: params.args.data.posts.create,
          },
        })
      );
    });

    it("can modify nested create action to be an upsert", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "create") {
            return params.query(
              {
                where: { id: params.args.id },
                create: params.args,
                update: params.args,
              },
              "upsert"
            );
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
            create: {
              title: faker.lorem.sentence(),
            },
          },
        },
      });
      const createData = params.args.data.posts.create;
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          upsert: {
            where: { id: createData.id },
            create: createData,
            update: createData,
          },
        })
      );
    });

    it("can modify nested create action to be a createMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "create") {
            return params.query({ data: [params.args] }, "createMany");
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
            create: { title: faker.lorem.sentence() },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          createMany: { data: [params.args.data.posts.create] },
        })
      );
    });

    it("can modify nested create action array to be a createMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "create") {
            return params.query(
              {
                data: [params.args],
              },
              "createMany"
            );
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
              { title: faker.lorem.sentence() },
              { title: faker.lorem.sentence() },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          createMany: { data: params.args.data.posts.create },
        })
      );
    });

    it("can modify nested create action to be a connectOrCreate", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "create") {
            return params.query(
              {
                where: { id: params.args.id },
                create: params.args,
              },
              "connectOrCreate"
            );
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
            create: {
              id: faker.datatype.number(),
              title: faker.lorem.sentence(),
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          connectOrCreate: {
            where: { id: params.args.data.posts.create.id },
            create: params.args.data.posts.create,
          },
        })
      );
    });
  });

  describe("update", () => {
    it("can modify nested update action to be a create", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.model === "Post") {
            return params.query(params.args.data, "create");
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
            update: {
              where: { id: faker.datatype.number() },
              data: { title: faker.lorem.sentence() },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          create: params.args.data.posts.update.data,
        })
      );
    });

    it("can modify nested update action to be a createMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.model === "Post") {
            return params.query({ data: [params.args.data] }, "createMany");
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
            update: {
              where: { id: faker.datatype.number() },
              data: { title: faker.lorem.sentence() },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          createMany: { data: [params.args.data.posts.update.data] },
        })
      );
    });

    it("can modify nested update action to be a connectOrCreate", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.model === "Post") {
            return params.query(
              {
                where: { id: params.args.where.id },
                create: params.args.data,
              },
              "connectOrCreate"
            );
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
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          connectOrCreate: {
            where: params.args.data.posts.update.where,
            create: params.args.data.posts.update.data,
          },
        })
      );
    });

    it("can modify nested update action to be an updateMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.model === "Post") {
            return params.query(params.args, "updateMany");
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
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          updateMany: params.args.data.posts.update,
        })
      );
    });

    it("can modify nested update action to be an upsert", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.model === "Post") {
            return params.query(
              {
                where: { id: params.args.where.id },
                create: params.args.data,
                update: params.args.data,
              },
              "upsert"
            );
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
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });
      const updateId = params.args.data.posts.update.where.id;
      const updateData = params.args.data.posts.update.data;

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          upsert: {
            where: { id: updateId },
            create: updateData,
            update: updateData,
          },
        })
      );
    });

    it("can modify nested update action to be a delete", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.model === "Post") {
            return params.query(
              {
                where: { id: params.args.where.id },
              },
              "delete"
            );
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
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          delete: {
            where: params.args.data.posts.update.where,
          },
        })
      );
    });

    it("can modify nested update action to be a deleteMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.model === "Post") {
            return params.query(
              { where: { id: params.args.where.id } },
              "deleteMany"
            );
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
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          deleteMany: {
            where: params.args.data.posts.update.where,
          },
        })
      );
    });
  });

  describe("upsert", () => {
    it("can modify nested upsert action to be a create", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "upsert") {
            return params.query(params.args.create, "create");
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
            upsert: {
              where: { id: faker.datatype.number() },
              create: {
                title: faker.lorem.sentence(),
              },
              update: {
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          create: params.args.data.posts.upsert.create,
        })
      );
    });

    it("can modify nested upsert action array to be a create array", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "upsert") {
            return params.query(params.args.create, "create");
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
            upsert: [
              {
                where: { id: faker.datatype.number() },
                create: { title: faker.lorem.sentence() },
                update: { title: faker.lorem.sentence() },
              },
              {
                where: { id: faker.datatype.number() },
                create: { title: faker.lorem.sentence() },
                update: { title: faker.lorem.sentence() },
              },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          create: params.args.data.posts.upsert.map((u: any) => u.create),
        })
      );
    });

    it("can modify nested upsert action to be a createMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "upsert") {
            return params.query({ data: [params.args.create] }, "createMany");
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
            upsert: [
              {
                where: { id: faker.datatype.number() },
                create: { title: faker.lorem.sentence() },
                update: { title: faker.lorem.sentence() },
              },
              {
                where: { id: faker.datatype.number() },
                create: { title: faker.lorem.sentence() },
                update: { title: faker.lorem.sentence() },
              },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          createMany: {
            data: params.args.data.posts.upsert.map(
              ({ create }: any) => create
            ),
          },
        })
      );
    });

    it("can modify nested upsert action to be a update", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "upsert") {
            return params.query(
              {
                where: { id: params.args.where.id },
                data: params.args.update,
              },
              "update"
            );
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
            upsert: [
              {
                where: { id: faker.datatype.number() },
                create: { title: faker.lorem.sentence() },
                update: { title: faker.lorem.sentence() },
              },
              {
                where: { id: faker.datatype.number() },
                create: { title: faker.lorem.sentence() },
                update: { title: faker.lorem.sentence() },
              },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: params.args.data.posts.upsert.map(
            ({ where, update }: any) => ({ where, data: update })
          ),
        })
      );
    });

    it("can modify nested upsert action to be a updateMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "upsert") {
            return params.query(
              {
                where: params.args.where,
                data: params.args.update,
              },
              "updateMany"
            );
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
            upsert: [
              {
                where: { id: faker.datatype.number() },
                create: { title: faker.lorem.sentence() },
                update: { title: faker.lorem.sentence() },
              },
              {
                where: { id: faker.datatype.number() },
                create: { title: faker.lorem.sentence() },
                update: { title: faker.lorem.sentence() },
              },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          updateMany: params.args.data.posts.upsert.map(
            ({ where, update }: any) => ({ where, data: update })
          ),
        })
      );
    });

    it("can modify nested upsert action to be a connectOrCreate", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "upsert") {
            return params.query(
              {
                where: params.args.where,
                create: params.args.create,
              },
              "connectOrCreate"
            );
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
            upsert: [
              {
                where: { id: faker.datatype.number() },
                create: { title: faker.lorem.sentence() },
                update: { title: faker.lorem.sentence() },
              },
              {
                where: { id: faker.datatype.number() },
                create: { title: faker.lorem.sentence() },
                update: { title: faker.lorem.sentence() },
              },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          connectOrCreate: params.args.data.posts.upsert.map(
            ({ where, create }: any) => ({ where, create })
          ),
        })
      );
    });
  });

  describe("createMany", () => {
    it("can modify nested createMany action to be a create list", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "createMany") {
            return params.query(params.args.data, "create");
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
            createMany: {
              data: [
                { title: faker.lorem.sentence() },
                { title: faker.lorem.sentence() },
              ],
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          create: params.args.data.posts.createMany.data,
        })
      );
    });

    it("can modify nested createMany action to be an update list", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "createMany") {
            return params.query(
              params.args.data.map((data: any) => ({
                where: { id: data.id },
                data,
              })),
              "update"
            );
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
            createMany: {
              data: [
                { id: 1, title: faker.lorem.sentence() },
                { id: 2, title: faker.lorem.sentence() },
              ],
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: params.args.data.posts.createMany.data.map((data: any) => ({
            where: { id: data.id },
            data,
          })),
        })
      );
    });

    it("can modify nested createMany action to be an upsert list", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "createMany") {
            return params.query(
              params.args.data.map((data: any) => ({
                where: { id: data.id },
                create: data,
                update: data,
              })),
              "upsert"
            );
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
            createMany: {
              data: [
                { id: 1, title: faker.lorem.sentence() },
                { id: 2, title: faker.lorem.sentence() },
              ],
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          upsert: params.args.data.posts.createMany.data.map((data: any) => ({
            where: { id: data.id },
            create: data,
            update: data,
          })),
        })
      );
    });

    it("can modify nested createMany action to be an connect list", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "createMany") {
            return params.query(
              params.args.data.map((data: any) => ({
                id: data.id,
              })),
              "connect"
            );
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
            createMany: {
              data: [
                { id: 1, title: faker.lorem.sentence() },
                { id: 2, title: faker.lorem.sentence() },
              ],
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          connect: params.args.data.posts.createMany.data.map((data: any) => ({
            id: data.id,
          })),
        })
      );
    });

    it("can modify nested createMany action to be a connectOrCreate list", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "createMany") {
            return params.query(
              params.args.data.map((data: any) => ({
                where: { id: data.id },
                create: data,
              })),
              "connectOrCreate"
            );
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
            createMany: {
              data: [
                { id: 1, title: faker.lorem.sentence() },
                { id: 2, title: faker.lorem.sentence() },
              ],
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          connectOrCreate: params.args.data.posts.createMany.data.map(
            (data: any) => ({
              where: { id: data.id },
              create: data,
            })
          ),
        })
      );
    });
  });

  describe("updateMany", () => {});

  describe("delete", () => {
    it("can modify nested delete action to be an update", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete") {
            return params.query(
              {
                where: { id: params.args.id },
                data: { deleted: true },
              },
              "update"
            );
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
            delete: { id: faker.datatype.number() },
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: {
            where: { id: params.args.data.posts.delete.id },
            data: { deleted: true },
          },
        })
      );
    });

    it("can modify nested delete action list to be an update list", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete") {
            return params.query(
              {
                where: { id: params.args.id },
                data: { deleted: true },
              },
              "update"
            );
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: {
          id: faker.datatype.number(),
        },
        data: {
          email: faker.internet.email(),
          posts: {
            delete: [
              { id: faker.datatype.number() },
              { id: faker.datatype.number() },
            ],
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: params.args.data.posts.delete.map((del: any) => ({
            where: del,
            data: { deleted: true },
          })),
        })
      );
    });

    it("can modify nested boolean delete action to be an update", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete" && params.args === true) {
            return params.query(
              {
                ...params.args,
                deleted: true,
              },
              "update"
            );
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: {
          id: faker.datatype.number(),
        },
        data: {
          email: faker.internet.email(),
          profile: {
            delete: true,
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.profile", { update: { deleted: true } })
      );
    });

    it("can modify deeply nested delete action to be an update", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete") {
            return params.query(
              {
                where: { id: params.args.id },
                data: { deleted: true },
              },
              "update"
            );
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
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
                comments: {
                  delete: { id: faker.datatype.number() },
                },
              },
            },
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts.update.data.comments", {
          update: {
            where: params.args.data.posts.update.data.comments.delete,
            data: { deleted: true },
          },
        })
      );
    });

    it("can modify deeply nested delete action to be an updateMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete") {
            return params.query(
              {
                where: { id: params.args.id },
                data: { deleted: true },
              },
              "updateMany"
            );
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
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
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
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts.update.data.comments", {
          updateMany: params.args.data.posts.update.data.comments.delete.map(
            (del: any) => ({
              where: del,
              data: { deleted: true },
            })
          ),
        })
      );
    });

    it("can modify deeply nested delete action list to be a deleteMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete") {
            return params.query(params.args, "deleteMany");
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
            update: {
              where: { id: faker.datatype.number() },
              data: {
                title: faker.lorem.sentence(),
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
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts.update.data.comments", {
          deleteMany: params.args.data.posts.update.data.comments.delete,
        })
      );
    });
  });

  describe("deleteMany", () => {
    it("can modify nested deleteMany action to be a delete", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "deleteMany") {
            return params.query(params.args, "delete");
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
            delete: [
              {
                id: faker.datatype.number(),
              },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          delete: params.args.data.posts.delete,
        })
      );
    });

    it("can modify nested deleteMany action to be an update", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "deleteMany") {
            return params.query(
              {
                where: params.args,
                data: { deleted: true },
              },
              "update"
            );
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
            deleteMany: [
              { id: faker.datatype.number() },
              { id: faker.datatype.number() },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: params.args.data.posts.deleteMany.map((del: any) => ({
            where: del,
            data: { deleted: true },
          })),
        })
      );
    });

    it("can modify nested deleteMany action to be an updateMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "deleteMany") {
            return params.query(
              {
                where: params.args,
                data: { deleted: true },
              },
              "updateMany"
            );
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
            deleteMany: [
              { id: faker.datatype.number() },
              { id: faker.datatype.number() },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          updateMany: params.args.data.posts.deleteMany.map((del: any) => ({
            where: del,
            data: { deleted: true },
          })),
        })
      );
    });
  });

  describe("connect", () => {
    it("can modify nested connect action to be a create", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connect") {
            return params.query(params.args, "create");
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
            connect: {
              id: faker.datatype.number(),
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          create: params.args.data.posts.connect,
        })
      );
    });

    it("can modify nested connect action to be a createMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connect") {
            params.query(
              {
                data: [params.args],
              },
              "createMany"
            );
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
            connect: {
              id: faker.datatype.number(),
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          createMany: {
            data: [params.args.data.posts.connect],
          },
        })
      );
    });

    it("can modify nested connect action to be an update", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connect") {
            return params.query(
              {
                where: { id: params.args.id },
                data: params.args,
              },
              "update"
            );
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
            connect: {
              id: faker.datatype.number(),
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: {
            where: { id: params.args.data.posts.connect.id },
            data: params.args.data.posts.connect,
          },
        })
      );
    });

    it("can modify nested connect action to be an updateMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connect") {
            return params.query(
              {
                where: { id: params.args.id },
                data: params.args,
              },
              "updateMany"
            );
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
            connect: {
              id: faker.datatype.number(),
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          updateMany: {
            where: { id: params.args.data.posts.connect.id },
            data: params.args.data.posts.connect,
          },
        })
      );
    });

    it("can modify nested connect action to be an upsert", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connect") {
            return params.query(
              {
                where: { id: params.args.id },
                update: params.args,
                create: params.args,
              },
              "upsert"
            );
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
            connect: {
              id: faker.datatype.number(),
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          upsert: {
            where: { id: params.args.data.posts.connect.id },
            create: params.args.data.posts.connect,
            update: params.args.data.posts.connect,
          },
        })
      );
    });

    it("can modify nested connect action to be an connectOrCreate", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connect") {
            return params.query(
              {
                where: { id: params.args.id },
                create: params.args,
              },
              "connectOrCreate"
            );
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
            connect: {
              id: faker.datatype.number(),
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          connectOrCreate: {
            where: { id: params.args.data.posts.connect.id },
            create: params.args.data.posts.connect,
          },
        })
      );
    });
  });

  describe("connectOrCreate", () => {
    it("can modify nested connectOrCreate action to be a create", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connectOrCreate") {
            return params.query(params.args.create, "create");
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
            connectOrCreate: {
              where: { id: faker.datatype.number() },
              create: {
                id: faker.datatype.number(),
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          create: params.args.data.posts.connectOrCreate.create,
        })
      );
    });

    it("can modify nested connectOrCreate action to be a createMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connectOrCreate") {
            return params.query(
              {
                data: [params.args.create],
              },
              "createMany"
            );
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
            connectOrCreate: {
              where: { id: faker.datatype.number() },
              create: {
                id: faker.datatype.number(),
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          createMany: {
            data: [params.args.data.posts.connectOrCreate.create],
          },
        })
      );
    });

    it("can modify nested connectOrCreate action to be an update", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connectOrCreate") {
            return params.query(
              {
                where: params.args.where,
                data: params.args.create,
              },
              "update"
            );
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
            connectOrCreate: {
              where: { id: faker.datatype.number() },
              create: {
                id: faker.datatype.number(),
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: {
            where: params.args.data.posts.connectOrCreate.where,
            data: params.args.data.posts.connectOrCreate.create,
          },
        })
      );
    });

    it("can modify nested connectOrCreate action to be an updateMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connectOrCreate") {
            return params.query(
              {
                where: params.args.where,
                data: params.args.create,
              },
              "updateMany"
            );
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
            connectOrCreate: {
              where: { id: faker.datatype.number() },
              create: {
                id: faker.datatype.number(),
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          updateMany: {
            where: params.args.data.posts.connectOrCreate.where,
            data: params.args.data.posts.connectOrCreate.create,
          },
        })
      );
    });

    it("can modify nested connectOrCreate action to be an upsert", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connectOrCreate") {
            return params.query(
              {
                where: params.args.where,
                create: params.args.create,
                update: params.args.create,
              },
              "upsert"
            );
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
            connectOrCreate: {
              where: { id: faker.datatype.number() },
              create: {
                id: faker.datatype.number(),
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          upsert: {
            where: params.args.data.posts.connectOrCreate.where,
            create: params.args.data.posts.connectOrCreate.create,
            update: params.args.data.posts.connectOrCreate.create,
          },
        })
      );
    });

    it("can modify nested connectOrCreate action to be an connect", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "connectOrCreate") {
            return params.query(params.args.where, "connect");
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
            connectOrCreate: {
              where: { id: faker.datatype.number() },
              create: {
                id: faker.datatype.number(),
                title: faker.lorem.sentence(),
              },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          connect: params.args.data.posts.connectOrCreate.where,
        })
      );
    });
  });

  describe("include", () => {
    it("can modify nested include action to be a select", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "include") {
            return params.query(params.args, "select");
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        include: {
          posts: true,
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith({
        where: params.args.where,
        select: {
          posts: true,
        },
      });
    });
  });

  describe("select", () => {
    it("can modify nested select action to be an include", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "select") {
            return params.query(params.args, "include");
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        include: {
          posts: {
            select: {
              author: true,
            },
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "include.posts", { include: { author: true } })
      );
    });
  });

  describe("merging", () => {
    it("merges converted write action args with existing write action args", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete") {
            return params.query(
              {
                where: { id: params.args.id },
                data: { deleted: true },
              },
              "update"
            );
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: {
          id: faker.datatype.number(),
        },
        data: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: { title: faker.lorem.sentence() },
            },
            delete: {
              id: faker.datatype.number(),
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: [
            {
              where: { id: params.args.data.posts.update.where.id },
              data: { title: params.args.data.posts.update.data.title },
            },
            {
              where: { id: params.args.data.posts.delete.id },
              data: { deleted: true },
            },
          ],
        })
      );
    });

    it("merges converted write array args with existing write action args", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete") {
            return params.query(
              {
                where: { id: params.args.id },
                data: { deleted: true },
              },
              "update"
            );
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: {
          id: faker.datatype.number(),
        },
        data: {
          email: faker.internet.email(),
          posts: {
            update: {
              where: { id: faker.datatype.number() },
              data: { title: faker.lorem.sentence() },
            },
            delete: [
              { id: faker.datatype.number() },
              { id: faker.datatype.number() },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: [
            {
              where: { id: params.args.data.posts.update.where.id },
              data: params.args.data.posts.update.data,
            },
            {
              where: { id: params.args.data.posts.delete[0].id },
              data: { deleted: true },
            },
            {
              where: { id: params.args.data.posts.delete[1].id },
              data: { deleted: true },
            },
          ],
        })
      );
    });

    it("merges converted write args with existing write action array args", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete") {
            return params.query(
              {
                where: { id: params.args.id },
                data: { deleted: true },
              },
              "update"
            );
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
                data: { title: faker.lorem.sentence() },
              },
            ],
            delete: { id: faker.datatype.number() },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: [
            {
              where: { id: params.args.data.posts.update[0].where.id },
              data: params.args.data.posts.update[0].data,
            },
            {
              where: { id: params.args.data.posts.delete.id },
              data: { deleted: true },
            },
          ],
        })
      );
    });

    it("merges converted write array args with existing write action array args", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete") {
            return params.query(
              {
                where: { id: params.args.id },
                data: { deleted: true },
              },
              "update"
            );
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: {
          id: faker.datatype.number(),
        },
        data: {
          email: faker.internet.email(),
          posts: {
            update: [
              {
                where: { id: faker.datatype.number() },
                data: { title: faker.lorem.sentence() },
              },
            ],
            delete: [
              { id: faker.datatype.number() },
              { id: faker.datatype.number() },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          update: [
            {
              where: { id: params.args.data.posts.update[0].where.id },
              data: params.args.data.posts.update[0].data,
            },
            {
              where: { id: params.args.data.posts.delete[0].id },
              data: { deleted: true },
            },
            {
              where: { id: params.args.data.posts.delete[1].id },
              data: { deleted: true },
            },
          ],
        })
      );
    });

    it("merges converted write args with existing write action args when nested in action array", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "delete") {
            return params.query(
              {
                where: { id: params.args.id },
                data: { deleted: true },
              },
              "update"
            );
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: {
          id: faker.datatype.number(),
        },
        data: {
          email: faker.internet.email(),
          posts: {
            update: [
              {
                where: { id: faker.datatype.number() },
                data: {
                  title: faker.lorem.sentence(),
                  comments: {
                    update: {
                      where: { id: faker.datatype.number() },
                      data: { content: "test comment content" },
                    },
                    delete: { id: faker.datatype.number() },
                  },
                },
              },
            ],
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts.update.0.data.comments", {
          update: [
            params.args.data.posts.update[0].data.comments.update,
            {
              where: params.args.data.posts.update[0].data.comments.delete,
              data: { deleted: true },
            },
          ],
        })
      );
    });

    it("merges operations converted to createMany with existing createMany", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "upsert") {
            return params.query(
              {
                data: [
                  {
                    title: params.args.create.title,
                    number: params.args.create.title.includes("first") ? 1 : 2,
                  },
                ],
              },
              "createMany"
            );
          }

          if (params.operation === "create") {
            return params.query(
              {
                data: [
                  {
                    title: params.args.title,
                    number: params.args.title.includes("first") ? 1 : 2,
                  },
                ],
              },
              "createMany"
            );
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
            create: [{ title: "first-create" }, { title: "second-create" }],
            createMany: {
              data: [{ title: "pre-existing" }],
            },
            upsert: [
              {
                where: { id: faker.datatype.number() },
                create: { title: "first-upsert" },
                update: { title: "first-upsert" },
              },
              {
                where: { id: faker.datatype.number() },
                create: { title: "second-upsert" },
                update: { title: "second-upsert" },
              },
            ],
          },
        },
      });
      await allOperations(params);

      // spread data here as a fix to: https://github.com/facebook/jest/issues/8475
      expect(query).toHaveBeenCalledTimes(1);
      expect([...query.mock.calls[0][0].data.posts.createMany.data]).toEqual([
        { title: "pre-existing" },
        { title: "first-create", number: 1 },
        { title: "second-create", number: 2 },
        { title: "first-upsert", number: 1 },
        { title: "second-upsert", number: 2 },
      ]);
    });

    it("merges operations converted to upsert action on toOne relation with existing upsert action", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "create" && params.scope) {
            return params.query(
              {
                create: params.args,
              },
              "upsert"
            );
          }
          if (params.operation === "update" && params.scope) {
            return params.query(
              {
                update: params.args,
              },
              "upsert"
            );
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            create: { bio: faker.lorem.sentence(), age: 20 },
            update: { bio: faker.lorem.sentence(), age: 30 },
            upsert: {
              create: { bio: faker.lorem.sentence() },
              update: { bio: faker.lorem.sentence(), age: 40 },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.profile", {
          upsert: {
            create: {
              ...params.args.data.profile.upsert.create,
              ...params.args.data.profile.create,
            },
            update: {
              ...params.args.data.profile.upsert.update,
              ...params.args.data.profile.update,
            },
          },
        })
      );
    });

    it("merges operations converted to create action on toOne relation with existing create action", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "upsert") {
            return params.query(params.args.create, "create");
          }
          if (params.operation === "update" && params.scope) {
            return params.query(params.args, "create");
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            update: { bio: faker.lorem.sentence(), age: 30 },
            create: { bio: faker.lorem.sentence(), age: 20 },
            upsert: {
              create: { bio: faker.lorem.sentence() },
              update: { bio: faker.lorem.sentence(), age: 40 },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.profile", {
          create: {
            ...params.args.data.profile.create,
            ...params.args.data.profile.update,
            ...params.args.data.profile.upsert.create,
          },
        })
      );
    });

    it("merges operations converted to update action on toOne relation with existing update action", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "upsert") {
            return params.query(params.args.update, "update");
          }

          if (params.operation === "create" && params.scope) {
            return params.query(params.args.data, "update");
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            create: { bio: faker.lorem.sentence(), age: 20 },
            update: { bio: faker.lorem.sentence(), age: 30 },
            upsert: {
              create: { bio: faker.lorem.sentence() },
              update: { bio: faker.lorem.sentence(), age: 40 },
            },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.profile", {
          update: {
            ...params.args.data.profile.update,
            ...params.args.data.profile.create,
            ...params.args.data.profile.upsert.update,
          },
        })
      );
    });

    it("merges connect action on toOne relation with existing connect action", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "create") {
            return params.query(
              {
                id: params.args.id,
              },
              "connect"
            );
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            create: { id: faker.datatype.number() },
            connect: { id: faker.datatype.number() },
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.profile", {
          connect: { id: params.args.data.profile.create.id },
        })
      );
    });

    it("merges connectOrCreate action on toOne relation with existing connectOrCreate action", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "create") {
            return params.query({ create: params.args }, "connectOrCreate");
          }

          if (params.operation === "update" && params.scope) {
            return params.query({ where: params.args }, "connectOrCreate");
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            create: {
              bio: faker.lorem.sentence(),
              age: 20,
            },
            connectOrCreate: {
              where: { id: faker.datatype.number() },
              create: { bio: faker.lorem.sentence() },
            },
            update: {
              id: faker.datatype.number(),
            },
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.profile", {
          connectOrCreate: {
            where: params.args.data.profile.update,
            create: {
              ...params.args.data.profile.connectOrCreate.create,
              ...params.args.data.profile.create,
            },
          },
        })
      );
    });

    it("merges disconnect where action with existing disconnect action", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "upsert") {
            return params.query(params.args.where, "disconnect");
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
            disconnect: {
              id: faker.datatype.number(),
            },
            upsert: [
              {
                where: { id: faker.datatype.number() },
                create: { title: "first-upsert" },
                update: { title: "first-upsert" },
              },
              {
                where: { id: faker.datatype.number() },
                create: { title: "second-upsert" },
                update: { title: "second-upsert" },
              },
            ],
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.posts", {
          disconnect: [
            params.args.data.posts.disconnect,
            get(params, "args.data.posts.upsert.0.where"),
            get(params, "args.data.posts.upsert.1.where"),
          ],
        })
      );
    });

    it("replaces existing disconnect true action with new disconnect action", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.scope) {
            return params.query(false, "disconnect");
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            disconnect: true,
            update: {
              bio: faker.lorem.sentence(),
            },
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.profile", { disconnect: false })
      );
    });

    it("replaces existing disconnect false action with new disconnect action", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.scope) {
            return params.query(true, "disconnect");
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            disconnect: false,
            update: {
              bio: faker.lorem.sentence(),
            },
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.profile", { disconnect: true })
      );
    });

    it("replaces existing delete true action with new delete action", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.scope) {
            return params.query(true, "delete");
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            delete: false,
            update: {
              bio: faker.lorem.sentence(),
            },
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.profile", { delete: true })
      );
    });

    it("replaces existing delete false action with new delete action", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "update" && params.scope) {
            return params.query(false, "delete");
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "update", {
        where: { id: faker.datatype.number() },
        data: {
          email: faker.internet.email(),
          profile: {
            delete: true,
            update: {
              bio: faker.lorem.sentence(),
            },
          },
        },
      });
      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "data.profile", { delete: false })
      );
    });

    it("replaces existing include with select changed to include", async () => {
      const allOperations = withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.operation === "select") {
            return params.query(params.args, "include");
          }

          return params.query(params.args);
        },
      });

      const query = jest.fn((_: any) => Promise.resolve(null));
      const params = createParams(query, "User", "findUnique", {
        where: { id: faker.datatype.number() },
        include: {
          posts: {
            select: { deleted: true },
            include: { author: true },
          },
        },
      });

      await allOperations(params);

      expect(query).toHaveBeenCalledWith(
        set(params.args, "include.posts", {
          include: { deleted: true },
        })
      );
    });
  });
});
