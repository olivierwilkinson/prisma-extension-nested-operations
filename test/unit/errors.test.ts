import faker from "faker";

import { withNestedOperations } from "../../src";
import { createParams } from "./helpers/createParams";
import { wait } from "./helpers/wait";

async function createAsyncError() {
  await wait(100);
  throw new Error("oops");
}

describe("errors", () => {
  it("throws when error encountered while modifying root params", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: async (params) => {
        await createAsyncError();
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        return params.query(params.args);
      },
    });

    const query = (_: any) => Promise.resolve({});
    const params = createParams(query, "User", "create", {
      data: { email: faker.internet.email() },
    });
    await expect(() => allOperations(params)).rejects.toThrow("oops");
  });

  it("throws when error encountered while modifying nested params", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: async (params) => {
        if (params.model === "Post") {
          await createAsyncError();
        }
        return params.query(params.args);
      },
    });

    const query = (_: any) => Promise.resolve({});
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        posts: {
          create: { title: faker.lorem.sentence() },
        },
      },
    });

    await expect(() => allOperations(params)).rejects.toThrow("oops");
  });

  it("throws if next encounters an error", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: (params) => {
        return params.query(params.args);
      },
    });

    const query = jest.fn(() => {
      return createAsyncError();
    });
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        posts: {
          create: { title: faker.lorem.sentence() },
        },
      },
    });

    await expect(() => allOperations(params)).rejects.toThrow("oops");
  });

  it("throws if error encountered modifying root result", async () => {
    const allOperations = withNestedOperations({
      $allNestedOperations: (params) => {
        return params.query(params.args);
      },
      $rootOperation: async (params) => {
        const result = await params.query(params.args);
        await createAsyncError();
        return result;
      },
    });

    const query = (_: any) => Promise.resolve({});
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
      },
    });

    await expect(() => allOperations(params)).rejects.toThrow("oops");
  });

  it("throws if error encountered modifying nested result", async () => {
    const allOperations = withNestedOperations({
      $rootOperation: (params) => {
        return params.query(params.args);
      },
      $allNestedOperations: async (params) => {
        const result = await params.query(params.args);
        if (params.model === "Post") {
          await createAsyncError();
        }
        return result;
      },
    });

    const query = (_: any) => Promise.resolve({});
    const params = createParams(query, "User", "create", {
      data: {
        email: faker.internet.email(),
        posts: {
          create: { title: faker.lorem.sentence() },
        },
      },
    });

    await expect(() => allOperations(params)).rejects.toThrow("oops");
  });
});
