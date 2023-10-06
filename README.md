<div align="center">
<h1>Prisma Extension Nested Operations</h1>

<p>Prisma Extension library that allows modifying operations on nested relation in a Prisma query.</p>

<p>
  Vanilla Prisma extensions are great for modifying top-level queries but
  are still difficult to use when they must handle
  <a href="https://www.prisma.io/docs/concepts/components/prisma-client/relation-queries#nested-writes">nested writes</a>, includes, selects,
  or modify where objects that reference relations.
  This is talked about in greater depth in this<a href="https://github.com/prisma/prisma/issues/4211">issue regarding nested middleware</a>, the
  same issues apply to extensions.
</p>

<p>
  This library exports a `withNestedOperations` helper that splits an `$allOperations()` hook into `$rootOperation()` and
  `$allNestedOperations()` hooks.
</p>

</div>

<hr />

[![Build Status][build-badge]][build]
[![version][version-badge]][package]
[![MIT License][license-badge]][license]
[![semantic-release](https://img.shields.io/badge/%20%20%F0%9F%93%A6%F0%9F%9A%80-semantic--release-e10079.svg)](https://github.com/semantic-release/semantic-release)
[![PRs Welcome][prs-badge]][prs]

## Table of Contents

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Installation](#installation)
- [Usage](#usage)
  - [`$rootOperation()`](#%5Crootoperation)
  - [`$allNestedOperations()` Params](#%5Callnestedoperations-params)
  - [Nested Writes](#nested-writes)
    - [Changing Nested Write Operations](#changing-nested-write-operations)
    - [Write Results](#write-results)
  - [Where](#where)
    - [Where Results](#where-results)
  - [Include](#include)
    - [Include Results](#include-results)
  - [Select](#select)
    - [Select Results](#select-results)
  - [Relations](#relations)
  - [Modifying Nested Write Params](#modifying-nested-write-params)
  - [Modifying Where Params](#modifying-where-params)
  - [Modifying Results](#modifying-results)
  - [Errors](#errors)
- [LICENSE](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Installation

This module is distributed via [npm][npm] which is bundled with [node][node] and
should be installed as one of your project's dependencies:

```
npm install prisma-extension-nested-operations
```

`@prisma/client` is a peer dependency of this library, so you will need to
install it if you haven't already:

```
npm install @prisma/client
```

You must have at least @prisma/client version 4.16.0 installed.

## Usage

The `withNestedOperations()` function takes and object with two properties, `$rootOperation()` and `$allNestedOperations()`.
The return value is an `$allOperations` hook, so it can be passed directly to an extensions `$allOperations` hook.

```javascript
import { withNestedOperations } from "prisma-extension-nested-operations";

client.$extends({
  query: {
    $allModels: {
      $allOperations: withNestedOperations({
        async $rootOperation(params) {
          // update root params here
          const result = params.query(params.args);
          // update root result here
          return result;
        },
        async $allNestedOperations(params) {
          // update nested params here
          const result = await params.query(params.args);
          // update nested result here
          return result;
        },
      }),
    },
  },
});
```

### `$rootOperation()`

The `$rootOperation()` hook is called with the same params as the `$allOperations` hook, however the `params.args` object
has been updated by the args passed to the `$allNestedOperations` query functions. The same pattern applies to the
returned result, it is the result of the query updated by the returned results from the `$allNestedOperations` calls.

### `$allNestedOperations` Params

The params object passed to the `$allNestedOperations` function is similar to the params passed to `$allOperations`.
It has `args`, `model`, `operation`, and `query` fields, however there are some key differences:

- the `operation` field adds the following options: 'connectOrCreate', 'connect', 'disconnect', 'include', 'select' and 'where'
- the `query` field takes a second argument, which is the `operation` being performed. This is useful where the type of the nested operation should be changed.
- there is an additional `scope` field that contains information specific to nested relations:

  - the `parentParams` field contains the params object of the parent relation
  - the `modifier` field contains any modifiers the params were wrapped in, for example `some` or `every`.
  - the `logicalOperators` field contains any logical operators between the current relation and it's parent, for example `AND` or `NOT`.
  - the `relations` field contains an object with the relation `to` the current model and `from` the model back to it's parent.

For more information on the `modifier` and `logicalOperators` fields see the [Where](#Where) section.

For more information on the `relations` field see the [Relations](#Relations) section.

The type for the params object is:

```typescript
type NestedParams<ExtArgs> = {
  query: (args: any, operation?: NestedOperation) => Prisma.PrismaPromise<any>;
  model: keyof Prisma.TypeMap<ExtArgs>["model"];
  args: any;
  operation: NestedOperation;
  scope?: Scope<ExtArgs>;
};

export type Scope<ExtArgs> = {
  parentParams: Omit<NestedParams<ExtArgs>, "query">;
  relations: { to: Prisma.DMMF.Field; from: Prisma.DMMF.Field };
  modifier?: Modifier;
  logicalOperators?: LogicalOperator[];
};

type Modifier = "is" | "isNot" | "some" | "none" | "every";

type LogicalOperator = "AND" | "OR" | "NOT";

type Operation =
  | "create"
  | "createMany"
  | "update"
  | "updateMany"
  | "upsert"
  | "delete"
  | "deleteMany"
  | "where"
  | "include"
  | "select"
  | "connect"
  | "connectOrCreate"
  | "disconnect";
```

### Nested Writes

The `$allNestedOperations()` function is called for every [nested write](https://www.prisma.io/docs/concepts/components/prisma-client/relation-queries#nested-writes)
operation in the query. The `operation` field is set to the operation being performed, for example "create" or "update".
The `model` field is set to the model being operated on, for example "User" or "Post".

For example take the following query:

```javascript
const result = await client.user.update({
  data: {
    posts: {
      update: {
        where: { id: 1 },
        data: { title: "Hello World" },
      },
    },
  },
});
```

The `$allNestedOperations()` function will be called with:

```javascript
{
  operation: 'update',
  model: 'Post',
  args: {
    where: { id: 1 },
    data: { title: 'Hello World' }
  },
  relations: {
    to: { kind: 'object', name: 'posts', isList: true, ... },
    from: { kind: 'object', name: 'author', isList: false, ... },
  },
  scope: [root params],
}
```

Some nested writes can be passed as an array of operations. In this case the `$allNestedOperations()` function is called for each
operation in the array. For example take the following query:

```javascript
const result = await client.user.update({
  data: {
    posts: {
      update: [
        { where: { id: 1 }, data: { title: "Hello World" } },
        { where: { id: 2 }, data: { title: "Hello World 2" } },
      ],
    },
  },
});
```

The `$allNestedOperations()` function will be called with:

```javascript
 {
  operation: 'update',
  model: 'Post',
  args: {
    where: { id: 1 },
    data: { title: 'Hello World' }
  },
  relations: {
    to: { kind: 'object', name: 'posts', isList: true, ... },
    from: { kind: 'object', name: 'author', isList: false, ... },
  },
  scope: [root params],
}
```

and

```javascript
 {
  operation: 'update',
  model: 'Post',
  args: {
    where: { id: 2 },
    data: { title: 'Hello World 2' }
  },
  relations: {
    to: { kind: 'object', name: 'posts', isList: true, ... },
    from: { kind: 'object', name: 'author', isList: false, ... },
  },
  scope: [root params],
}
```

#### Changing Nested Write Operations

The `$allNestedOperations()` function can change the operation that is performed on the model. For example take the following query:

```javascript
const result = await client.user.update({
  data: {
    posts: {
      update: {
        where: { id: 1 }
        data: { title: 'Hello World' }
      },
    },
  },
});
```

The `$allNestedOperations()` function could be used to change the operation to `upsert`:

```javascript
const client = _client.$extends({
  query: {
    $allModels: {
      $allOperations: withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.model === "Post" && params.operation === "update") {
            return params.query(
              {
                where: params.args.where,
                create: params.args.data,
                update: params.args.data,
              },
              "upsert"
            );
          }
          return params.query(params);
        },
      }),
    },
  },
});
```

The final query would be modified by the above `$allNestedOperations()` to:

```javascript
const result = await client.user.update({
  data: {
    posts: {
      upsert: {
        where: { id: 1 },
        create: { title: "Hello World" },
        update: { title: "Hello World" },
      },
    },
  },
});
```

When changing the operation it is possible for the operation to already exist. In this case the resulting operations are merged.
For example take the following query:

```javascript
const result = await client.user.update({
  data: {
    posts: {
      update: {
        where: { id: 1 },
        data: { title: "Hello World" },
      },
      upsert: {
        where: { id: 2 },
        create: { title: "Hello World 2" },
        update: { title: "Hello World 2" },
      },
    },
  },
});
```

Using the same `$allNestedOperations()` defined before the update operation would be changed to an upsert operation, however there is
already an upsert operation so the two operations are merged into a upsert operation array with the new operation added to
the end of the array. When the existing operation is already a list of operations the new operation is added to the end of
the list. The final query in this case would be:

```javascript
const result = await client.user.update({
  data: {
    posts: {
      upsert: [
        {
          where: { id: 2 },
          create: { title: "Hello World 2" },
          update: { title: "Hello World 2" },
        },
        {
          where: { id: 1 },
          create: { title: "Hello World" },
          update: { title: "Hello World" },
        },
      ],
    },
  },
});
```

Sometimes it is not possible to merge the operations together in this way. The `createMany` operation does not support
operation arrays so the `data` field of the `createMany` operation is merged instead. For example take the following query:

```javascript
const result = await client.user.create({
  data: {
    posts: {
      createMany: {
        data: [{ title: "Hello World" }, { title: "Hello World 2" }],
      },
      create: {
        title: "Hello World 3",
      },
    },
  },
});
```

If the `create` operation was changed to be a `createMany` operation the `data` field would be added to the end of the existing
`createMany` operation. The final query would be:

```javascript
const result = await client.user.create({
  data: {
    posts: {
      createMany: {
        data: [
          { title: "Hello World" },
          { title: "Hello World 2" },
          { title: "Hello World 3" },
        ],
      },
    },
  },
});
```

It is also not possible to merge the operations together by creating an array of operations for non-list relations. For
example take the following query:

```javascript
const result = await client.user.update({
  data: {
    profile: {
      create: {
        bio: "My personal bio",
        age: 30,
      },
      update: {
        where: { id: 1 },
        data: { bio: "Updated bio" },
      },
    },
  },
});
```

If the `update` operation was changed to be a `create` operation using the following extension:

```javascript
const client = _client.$extends({
  query: {
    $allModels: {
      $allOperations: withNestedOperations({
        $rootOperation: (params) => {
          return params.query(params.args);
        },
        $allNestedOperations: (params) => {
          if (params.model === "Profile" && params.operation === "update") {
            return params.query(params.args.data, "create");
          }
          return params.query(params);
        },
      }),
    },
  },
});
```

The `create` operation from the `update` operation would need be merged with the existing `create` operation, however since
`profile` is not a list relation we must merge together the resulting objects instead, resulting in the final query:

```javascript
const result = await client.user.create({
  data: {
    profile: {
      create: {
        bio: "Updated bio",
        age: 30,
      },
    },
  },
});
```

#### Write Results

The `query` function of `$allNestedOperations()` calls for nested write operations always return `undefined` as their result.
This is because the results returned from the root query may not include the data for a particular nested write.

For example take the following query:

```javascript
const result = await client.user.update({
  data: {
    profile: {
      create: {
        bio: "My personal bio",
        age: 30,
      },
    }
    posts: {
      updateMany: {
        where: {
          published: false,
        },
        data: {
          published: true,
        },
      },
    },
  },
  select: {
    id: true,
    posts: {
      where: {
        title: {
          contains: "Hello",
        },
      },
      select: {
        id: true,
      },
    },
  }
});
```

The `profile` field is not included in the `select` object so the result of the `create` operation will not be included in
the root result. The `posts` field is included in the `select` object but the `where` object only includes posts with
titles that contain "Hello" and returns only the "id" field, in this case it is not possible to match the result of the
`updateMany` operation to the returned Posts.

See [Modifying Results](#modifying-results) for more information on how to update the results of queries.

### Where

The `where` operation is called for any relations found inside where objects in params.

Note that the `where` operation is not called for the root where object, this is because you need the root operation to know
what properties the root where object accepts. For nested where objects this is not a problem as they always follow the
same pattern.

To see where the `where` operation is called take the following query:

```javascript
const result = await client.user.findMany({
  where: {
    posts: {
      some: {
        published: true,
      },
    },
  },
});
```

The where object above produces a call for "posts" relation found in the where object. The `modifier` field is set to
"some" since the where object is within the "some" field.

```javascript
{
  operation: 'where',
  model: 'Post',
  args: {
    published: true,
  },
  scope: {
    parentParams: {...}
    modifier: 'some',
    relations: {...}
  },
}
```

Relations found inside where AND, OR and NOT logical operators are also found and called with the `$allNestedOperations()` function,
however the `where` operation is not called for the logical operators themselves. For example take the following query:

```javascript
const result = await client.user.findMany({
  where: {
    posts: {
      some: {
        published: true,
        AND: [
          {
            title: "Hello World",
          },
          {
            comments: {
              every: {
                text: "Great post!",
              },
            },
          },
        ],
      },
    },
  },
});
```

The `$allNestedOperations()` function will be called with the params for "posts" similarly to before, however it will also be called
with the following params:

```javascript
{
  operation: 'where',
  model: 'Comment',
  args: {
    text: "Great post!",
  },
  scope: {
    parentParams: {...}
    modifier: 'every',
    logicalOperators: ['AND'],
    relations: {...}
  },
}
```

Since the "comments" relation is found inside the "AND" logical operator the
\$allNestedOperations is called for it. The `modifier` field is set to "every" since the where object is in the "every" field and
the `logicalOperators` field is set to `['AND']` since the where object is inside the "AND" logical operator.

Notice that the `$allNestedOperations()` function is not called for the first item in the "AND" array, this is because the first item
does not contain any relations.

The `logicalOperators` field tracks all the logical operators between the `parentParams` and the current params. For
example take the following query:

```javascript
const result = await client.user.findMany({
  where: {
    AND: [
      {
        NOT: {
          OR: [
            {
              posts: {
                some: {
                  published: true,
                },
              },
            },
          ],
        },
      },
    ],
  },
});
```

The `$allNestedOperations()` function will be called with the following params:

```javascript
{
  operation: 'where',
  model: 'Post',
  args: {
    published: true,
  },
  scope: {
    parentParams: {...}
    modifier: 'some',
    logicalOperators: ['AND', 'NOT', 'OR'],
    relations: {...},
  },
}
```

The `where` operation is also called for relations found in the `where` field of includes and selects. For example:

```javascript
const result = await client.user.findMany({
  select: {
    posts: {
      where: {
        published: true,
      },
    },
  },
});
```

The `$allNestedOperations()` function will be called with the following params:

```javascript
{
  operation: 'where',
  model: 'Post',
  args: {
    published: true,
  },
  scope: {...}
}
```

#### Where Results

The `query` function for a `where` operation always resolves with `undefined`.

### Include

The `include` operation will be called for any included relation. The `args` field will contain the object or boolean
passed as the relation include. For example take the following query:

```javascript
const result = await client.user.findMany({
  include: {
    profile: true,
    posts: {
      where: {
        published: true,
      },
    },
  },
});
```

For the "profile" relation the `$allNestedOperations()` function will be called with:

```javascript
{
  operation: 'include',
  model: 'Profile',
  args: true,
  scope: {...}
}
```

and for the "posts" relation the `$allNestedOperations()` function will be called with:

```javascript
{
  operation: 'include',
  model: 'Post',
  args: {
    where: {
      published: true,
    },
  },
  scope: {...}
}
```

#### Include Results

The `query` function for an `include` operation resolves with the result of the `include` operation. For example take the
following query:

```javascript
const result = await client.user.findMany({
  include: {
    profile: true,
  },
});
```

The `$allNestedOperations()` function for the "profile" relation will be called with:

```javascript
{
  operation: 'include',
  model: 'Profile',
  args: true,
  scope: {...}
}
```

And the `query` function will resolve with the result of the `include` operation, in this case something like:

```javascript
{
  id: 2,
  bio: 'My personal bio',
  age: 30,
  userId: 1,
}
```

For relations that are included within a list of parent results the `query` function will resolve with a flattened array
of all the models from each parent result. For example take the following query:

```javascript
const result = await client.user.findMany({
  include: {
    posts: true,
  },
});
```

If the root result looks like the following:

```javascript
[
  {
    id: 1,
    name: "Alice",
    posts: [
      {
        id: 1,
        title: "Hello World",
        published: false,
        userId: 1,
      },
      {
        id: 2,
        title: "My first published post",
        published: true,
        userId: 1,
      },
    ],
  },
  {
    id: 2,
    name: "Bob",
    posts: [
      {
        id: 3,
        title: "Clean Code",
        published: true,
        userId: 2,
      },
    ],
  },
];
```

The `query` function for the "posts" relation will resolve with the following:

```javascript
[
  {
    id: 1,
    title: "Hello World",
    published: false,
    userId: 1,
  },
  {
    id: 2,
    title: "My first published post",
    published: true,
    userId: 1,
  },
  {
    id: 3,
    title: "Clean Code",
    published: true,
    userId: 2,
  },
];
```

For more information on how to modify the results of an `include` operation see the [Modifying Results](#modifying-results)

### Select

Similarly to the `include` operation, the `select` operation will be called for any selected relation with the `args` field
containing the object or boolean passed as the relation select. For example take the following query:

```javascript
const result = await client.user.findMany({
  select: {
    posts: true,
    profile: {
      select: {
        bio: true,
      },
    },
  },
});
```

and for the "posts" relation the `$allNestedOperations()` function will be called with:

```javascript
{
  operation: 'select',
  model: 'Post',
  args: true,
  scope: {...}
}
```

For the "profile" relation the `$allNestedOperations()` function will be called with:

```javascript
{
  operation: 'select',
  model: 'Profile',
  args: {
    bio: true,
  },
  scope: {...}
}
```

#### Select Results

The `query` function for a `select` operation resolves with the result of the `select` operation. This is the same as the
`include` operation. See the [Include Results](#include-results) section for more information.

### Relations

The `relations` field of the `scope` object contains the relations relevant to the current model. For example take the
following query:

```javascript
const result = await client.user.create({
  data: {
    email: "test@test.com",
    profile: {
      create: {
        bio: "Hello World",
      },
    },
    posts: {
      create: {
        title: "Hello World",
      },
    },
  },
});
```

The `$allNestedOperations()` function will be called with the following params for the "profile" relation:

```javascript
{
  operation: 'create',
  model: 'Profile',
  args: {
    bio: "Hello World",
  },
  scope: {
    parentParams: {...}
    relations: {
      to: { name: 'profile', kind: 'object', isList: false, ... },
      from: { name: 'user', kind: 'object', isList: false, ... },
    },
  },
}
```

and the following params for the "posts" relation:

```javascript
{
  operation: 'create',
  model: 'Post',
  args: {
    title: "Hello World",
  },
  scope: {
    parentParams: {...}
    relations: {
      to: { name: 'posts', kind: 'object', isList: true, ... },
      from: { name: 'author', kind: 'object', isList: false, ... },
    },
  },
}
```

### Modifying Nested Write Params

When writing extensions that modify the params of a query you should first write the `$rootOperation()` hook as if it were
an `$allOperations()` hook, and then add the `$allNestedOperations()` hook.

Say you are writing middleware that sets a default value when creating a model for a particular model:

```javascript
const client = _client.$extends({
  query: {
    $allModels: {
      $allOperations: withNestedOperations({
        async $rootOperation(params) {
          // we only want to add default values for the "Invite" model
          if (params.model !== "Invite") {
            return params.query(params.args);
          }

          if (params.operation === "create" && !params.args.data.code) {
            params.args.data.code = createCode();
          }

          if (params.operation === "upsert" && !params.args.create.code) {
            params.args.create.code = createCode();
          }

          if (params.operation === "createMany") {
            params.args.data.forEach((data) => {
              if (!data.code) {
                data.code = createCode();
              }
            });
          }

          return params.query(params.args);
        },
        async $allNestedOperations(params) {
          return params.query(params.args);
        },
      }),
    },
  },
});
```

Then add conditions for the different args and operations that can be found in nested writes:

```javascript
const client = _client.$extends({
  query: {
    $allModels: {
      $allOperations: withNestedOperations({
        async $rootOperation(params) {
          [...]
        },
        async $allNestedOperations(params) {
          // we only want to add default values for the "Invite" model
          if (params.model !== "Invite") {
            return params.query(params.args);
          }

          // when the "create" operation is from a nested write the data is not in the "data" field
          if (params.operation === "create" && !params.args.code) {
            params.args.code = createCode();
          }

          // handle the "connectOrCreate" operation
          if (params.operation === "connectOrCreate" && !params.args.create.code) {
            params.args.create.code = createCode();
          }

          // pass args to query
          return params.query(params.args);
        },
      }),
    },
  },
});
```

### Modifying Where Params

When writing extensions that modify the where params of a query you should first write the `$rootOperation()` hook as
if it were an $allOperations hook, this is because the `where` operation is not called for the root where object and so you
will need to handle it manually.

Say you are writing an extension that excludes models with a particular field, let's call it "invisible" rather than
"deleted" to make this less familiar:

```javascript
const client = _client.$extends({
  query: {
    $allModels: {
      $allOperations: withNestedOperations({
        async $rootOperation(params) {
          // don't handle operations that only accept unique fields such as findUnique or upsert
          if (
            params.operation === "findFirst" ||
            params.operation === "findFirstOrThrow" ||
            params.operation === "findMany" ||
            params.operation === "updateMany" ||
            params.operation === "deleteMany" ||
            params.operation === "count" ||
            params.operation === "aggregate"
          ) {
            return params.query({
              ...params.args,
              where: {
                ...params.args.where,
                invisible: false,
              },
            });
          }

          return params.query(params.args);
        },
        async $allNestedOperations(params) {
          return params.query(params.args);
        },
      }),
    },
  },
});
```

Then add conditions for the `where` operation:

```javascript
const client = _client.$extends({
  query: {
    $allModels: {
      $allOperations: withNestedOperations({
        async $rootOperation(params) {
          [...]
        },
        async $allNestedOperations(params) {
          // handle the "where" operation
          if (params.operation === "where") {
            return params.query({
              ...params.args,
              invisible: false,
            });
          }

          return params.query(params.args);
        },
      }),
    },
  },
});
```

### Modifying Results

When writing extensions that modify the results of a query you should take the following process:

- handle all the root cases in the `$rootOperation()` hook the same way you would with a $allOperations hook.
- handle nested results using the `include` and `select` operations in the `$allNestedOperations()` hook.

Say you are writing middleware that adds a timestamp to the results of a query. You would first handle the root cases:

```javascript
const client = _client.$extends({
  query: {
    $allModels: {
      $allOperations: withNestedOperations({
        async $rootOperation(params) {
          const result = await params.query(params.args);

          // ensure result is defined
          if (!result) return result;

          // handle root operations
          if (
            params.operation === "findFirst" ||
            params.operation === "findFirstOrThrow" ||
            params.operation === "findUnique" ||
            params.operation === "findUniqueOrThrow" ||
            params.operation === "create" ||
            params.operation === "update" ||
            params.operation === "upsert" ||
            params.operation === "delete"
          ) {
            result.timestamp = Date.now();
            return result;
          }

          if (params.operation === "findMany") {
            const result = await params.query(params.args);
            result.forEach((model) => {
              model.timestamp = Date.now();
            });
            return result;
          }

          return result;
        },
        async $allNestedOperations(params) {
          return params.query(params.args);
        },
      }),
    },
  },
});
```

Then you would handle the nested results using the `include` and `select` operations:

```javascript
const client = _client.$extends({
  query: {
    $allModels: {
      $allOperations: withNestedOperations({
        async $rootOperation(params) {
          [...]
        },
        async $allNestedOperations(params) {
          const result = await next(params);

          // ensure result is defined
          if (!result) return result;

          // handle nested operations
          if (params.operation === "include" || params.operation === "select") {
            if (Array.isArray(result)) {
              result.forEach((model) => {
                model.timestamp = Date.now();
              });
            } else {
              result.timestamp = Date.now();
            }
            return result;
          }

          return result;
        },
      }),
    },
  },
});
```

You could also write the above middleware by creating new objects for each result rather than mutating the existing
objects:

```javascript
const client = _client.$extends({
  query: {
    $allModels: {
      $allOperations: withNestedOperations({
        async $rootOperation(params) {
          [...]
        },
        async $allNestedOperations(params) {
          const result = await next(params);

          // ensure result is defined
          if (!result) return result;

          // handle nested operations
          if (params.operation === "include" || params.operation === "select") {
            if (Array.isArray(result)) {
              return result.map((model) => ({
                ...model,
                timestamp: Date.now(),
              });
            }

            return {
              ...result,
              timestamp: Date.now(),
            };
          }

          return result;
        },
      }),
    },
  },
});
```

NOTE: When modifying results from `include` or `select` operations it is important to either mutate the existing objects or
spread the existing objects into the new objects. This is because `createNestedMiddleware` needs some fields from the
original objects in order to correct update the root results.

### Errors

If any middleware throws an error at any point then the root query will throw with that error. Any middleware that is
pending will have it's promises rejects at that point.

## LICENSE

Apache 2.0

[npm]: https://www.npmjs.com/
[node]: https://nodejs.org
[build-badge]: https://github.com/olivierwilkinson/prisma-extension-nested-operations/workflows/prisma-extension-nested-operations/badge.svg
[build]: https://github.com/olivierwilkinson/prisma-extension-nested-operations/actions?query=branch%3Amain+workflow%3Aprisma-extension-nested-operations
[version-badge]: https://img.shields.io/npm/v/prisma-extension-nested-operations.svg?style=flat-square
[package]: https://www.npmjs.com/package/prisma-extension-nested-operations
[downloads-badge]: https://img.shields.io/npm/dm/prisma-extension-nested-operations.svg?style=flat-square
[npmtrends]: http://www.npmtrends.com/prisma-extension-nested-operations
[license-badge]: https://img.shields.io/npm/l/prisma-extension-nested-operations.svg?style=flat-square
[license]: https://github.com/olivierwilkinson/prisma-extension-nested-operations/blob/master/LICENSE
[prs-badge]: https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square
[prs]: http://makeapullrequest.com
[coc-badge]: https://img.shields.io/badge/code%20of-conduct-ff69b4.svg?style=flat-square
[coc]: https://github.com/olivierwilkinson/prisma-extension-nested-operations/blob/master/other/CODE_OF_CONDUCT.md
