import { groupBy as rawGroupBy, mapValues } from "lodash-es";
export const groupBy = (arr, proj) =>
  mapValues(rawGroupBy(arr, proj), (arr) => [arr[0], arr.slice(1)]);
