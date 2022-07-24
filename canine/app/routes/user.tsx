import { Provider } from "react-redux";

import { User } from "~/components/User";
import store from "~/store";

export default function Index() {
  return (
    <Provider store={store}>
      <User />
    </Provider>
  );
}
