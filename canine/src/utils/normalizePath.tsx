export function normalizePath(path: string): string {
  const parts = path.split("/");
  const result = [];
  for (let i = 0; i < parts.length; i++) {
    const part = parts[i];
    if (part === "") continue;
    if (part === ".") continue;
    if (part === "..") {
      if (result.length !== 0) {
        result.pop();
      }
      continue;
    }
    result.push(part);
  }
  return result.join("/");
}
