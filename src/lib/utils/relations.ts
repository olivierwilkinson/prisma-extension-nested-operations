import { Prisma } from "@prisma/client";

if (!Prisma.dmmf) {
  throw new Error(
    "Prisma DMMF not found, please generate Prisma client using `npx prisma generate`"
  );
}

export const relationsByModel: Record<string, Prisma.DMMF.Field[]> = {};
Prisma.dmmf.datamodel.models.forEach((model: Prisma.DMMF.Model) => {
  relationsByModel[model.name] = model.fields.filter(
    (field) => field.kind === "object" && field.relationName
  );
});

export function findOppositeRelation(relation: Prisma.DMMF.Field) {
  const parentRelations =
    relationsByModel[relation.type as Prisma.ModelName] || [];

  const oppositeRelation = parentRelations.find(
    (parentRelation) =>
      parentRelation !== relation &&
      parentRelation.relationName === relation.relationName
  );

  if (!oppositeRelation) {
    throw new Error(`Unable to find opposite relation to ${relation.name}`);
  }

  return oppositeRelation;
}
