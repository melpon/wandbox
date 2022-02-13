import { configureStore, getDefaultMiddleware } from "@reduxjs/toolkit";
import { wandboxReducer } from "~/features/slice";
import { useStore, useDispatch } from "react-redux";

const store = configureStore({
  reducer: {
    wandbox: wandboxReducer,
  },
  middleware: (getDefaultMiddleware) =>
    getDefaultMiddleware({ serializableCheck: false }),
});
export type AppStore = typeof store;
export type AppState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;
export const useAppStore = (): AppStore => useStore();
export const useAppDispatch = (): AppDispatch => useDispatch<AppDispatch>();

export default store;
