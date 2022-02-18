import { Provider } from "react-redux";
import { Wandbox } from "~/components/Wandbox";
import store from "~/store";

export default function () {
  return (
    <Provider store={store}>
      <Wandbox />
    </Provider>
  );
}
