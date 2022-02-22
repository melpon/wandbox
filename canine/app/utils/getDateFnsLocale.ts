import type { Locale } from "date-fns";
import { enUS, ja } from "date-fns/locale";

export function getDateFnsLocale(language: string): Locale {
  if (language.startsWith("ja")) {
    return ja;
  }
  return enUS;
}
