import { AppContexts } from "~/apps/AppContexts";
import { Wandbox } from "~/apps/Wandbox";

export default function Index() {
  return (
    <AppContexts>
      <Wandbox />
    </AppContexts>
  );
}
