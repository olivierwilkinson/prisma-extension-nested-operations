const idSymbol = Symbol("id");
const parentIdSymbol = Symbol("parentId");

function addIdSymbolsToObject(
  obj: Record<string | symbol, any>,
  id: number,
  parentId?: number
) {
  obj[idSymbol] = id;
  if (parentId) {
    obj[parentIdSymbol] = parentId;
  }
}

function stripIdSymbolsFromObject(obj: Record<string | symbol, any>) {
  if (obj[idSymbol]) {
    delete obj[idSymbol];
  }
  if (obj[parentIdSymbol]) {
    delete obj[parentIdSymbol];
  }
}

export function addIdSymbolsToResult(
  result: any,
  parentId?: number,
  startId = 1
): number {
  let id = startId;

  if (Array.isArray(result)) {
    result.forEach((item) => {
      if (typeof item === "object" && item !== null) {
        addIdSymbolsToObject(item, id, parentId);
        id += 1;

        Object.getOwnPropertyNames(item).forEach((key) => {
          if (typeof item[key] === "object" && item[key] !== null) {
            id = addIdSymbolsToResult(item[key], item[idSymbol], id);
          }
        });
      }
    });

    return id;
  }

  if (typeof result === "object" && result !== null) {
    addIdSymbolsToObject(result, id, parentId);
    id += 1;

    Object.getOwnPropertyNames(result).forEach((key) => {
      if (typeof result[key] === "object" && result[key] !== null) {
        id = addIdSymbolsToResult(result[key], result[idSymbol], id);
      }
    });
  }

  return id;
}

export function stripIdSymbolsFromResult(result: any) {
  if (Array.isArray(result)) {
    result.forEach((item) => {
      if (typeof item === "object" && item !== null) {
        stripIdSymbolsFromObject(item);

        Object.getOwnPropertyNames(item).forEach((key) => {
          if (typeof item[key] === "object" && item[key] !== null) {
            stripIdSymbolsFromResult(item[key]);
          }
        });
      }
    });
    return;
  }

  if (typeof result === "object" && result !== null) {
    stripIdSymbolsFromObject(result);

    Object.getOwnPropertyNames(result).forEach((key) => {
      if (typeof result[key] === "object" && result[key] !== null) {
        stripIdSymbolsFromResult(result[key]);
      }
    });
  }
}

export function getRelationResult(result: any, relations: string[]): any {
  let relationResult = result;

  for (const relation of relations) {
    if (!relationResult) return;

    if (Array.isArray(relationResult)) {
      relationResult = relationResult
        .flatMap((item) => item[relation])
        .filter(Boolean);
    } else {
      relationResult = relationResult[relation];
    }
  }

  return relationResult;
}

function injectRelationResult(
  result: any,
  relation: string,
  relationResult: any
) {
  if (Array.isArray(relationResult) && Array.isArray(result[relation])) {
    result[relation] = relationResult.filter(
      (item) => item[parentIdSymbol] === result[idSymbol]
    );
    return;
  }

  if (Array.isArray(relationResult) && !Array.isArray(result[relation])) {
    result[relation] =
      relationResult.find(
        (item) => item[parentIdSymbol] === result[idSymbol]
      ) || null;
    return;
  }

  if (Array.isArray(result[relation])) {
    throw new Error("Cannot inject a single result into an array result");
  }

  result[relation] = relationResult;
}

export function updateResultRelation(
  result: any,
  relation: string,
  relationResult: any
) {
  if (Array.isArray(result)) {
    result.forEach((item) => {
      if (typeof item === "object" && item !== null && item[relation]) {
        injectRelationResult(item, relation, relationResult);
      }
    });
    
    return result;
  }

  if (typeof result === "object" && result !== null && result[relation]) {
    injectRelationResult(result, relation, relationResult);
  }

  return result;
}
