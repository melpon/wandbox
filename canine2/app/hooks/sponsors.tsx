import type { AnyJson } from "./fetch";
import { useFetchJSON } from "./fetch";

export interface Sponsor {
  name: string;
  url: string;
  due_date: number;
}
export interface SponsorsGetData {
  corporate: Sponsor[];
  personal: Sponsor[];
}

function resolveSponsorsGetData(json: AnyJson): SponsorsGetData {
  return json as unknown as SponsorsGetData;
}

export function useGetSponsors(
  url: string,
  onError: (error: string) => void
): [
    SponsorsGetData | null,
    number,
    (url: string | null, opts: RequestInit) => void
  ] {
  const headers = {
    "Content-Type": "application/json",
  };
  return useFetchJSON<SponsorsGetData>(
    url,
    { method: "GET", headers: headers },
    resolveSponsorsGetData,
    onError
  );
}
