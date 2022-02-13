import { createSlice, PayloadAction } from "@reduxjs/toolkit";

type ResultType =
  | "Control"
  | "CompilerMessageS"
  | "CompilerMessageE"
  | "StdOut"
  | "StdErr"
  | "ExitCode"
  | "Signal";

export interface ResultData {
  type: ResultType;
  data: string;
}

const initialState = {
  currentLanguage: "",
  currentCompilerName: "",
  currentSwitches: {} as { [name: string]: string | boolean },
  compilerOptionRaw: "",
  runtimeOptionRaw: "",
  results: [] as ResultData[],
};

export type WandboxState = typeof initialState;

export const wandboxSlice = createSlice({
  name: "wandbox",
  initialState: initialState,
  reducers: {
    setCurrentLanguage: (state, action: PayloadAction<string>) => {
      state.currentLanguage = action.payload;
    },
    setCurrentCompilerName: (state, action: PayloadAction<string>) => {
      state.currentCompilerName = action.payload;
    },
    setCurrentSwitch: (
      state,
      action: PayloadAction<{ switchName: string; value: boolean | string }>
    ) => {
      const { switchName, value } = action.payload;
      state.currentSwitches[switchName] = value;
    },
    setCurrentSwitches: (
      state,
      action: PayloadAction<WandboxState["currentSwitches"]>
    ) => {
      state.currentSwitches = action.payload;
    },
    setCompilerOptionRaw: (state, action: PayloadAction<string>) => {
      state.compilerOptionRaw = action.payload;
    },
    setRuntimeOptionRaw: (state, action: PayloadAction<string>) => {
      state.runtimeOptionRaw = action.payload;
    },
    clearResult: (state) => {
      state.results = [];
    },
    addResult: (state, action: PayloadAction<ResultData>) => {
      state.results.push(action.payload);
    },
    setResults: (state, action: PayloadAction<ResultData[]>) => {
      state.results = action.payload;
    },
  },
  extraReducers: (builder) => {},
});

export const wandboxReducer = wandboxSlice.reducer;
export const wandboxActions = wandboxSlice.actions;
