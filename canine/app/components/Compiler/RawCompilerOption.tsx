import React, { useEffect, useState } from "react";
import { Dropdown, Form } from "react-bootstrap";
import { ThreeDots } from "react-bootstrap-icons";
import { useTranslation } from "react-i18next";

interface RawCompilerOptionProps {
  enabledCompilerOptionRaw: boolean;
  compilerOptionRaw: string;
  enabledRuntimeOptionRaw: boolean;
  runtimeOptionRaw: string;
  readOnly: boolean;
  onChangeCompilerOptionRaw: (value: string) => void;
  onChangeRuntimeOptionRaw: (value: string) => void;
  onCtrlEnter?: () => void;
}

const RawCompilerOption: React.FC<RawCompilerOptionProps> = (
  props
): React.ReactElement => {
  const {
    enabledCompilerOptionRaw,
    compilerOptionRaw,
    enabledRuntimeOptionRaw,
    runtimeOptionRaw,
    readOnly,
    onChangeCompilerOptionRaw,
    onChangeRuntimeOptionRaw,
  } = props;
  const { t } = useTranslation();

  // コンパイル時オプションが有効であっても、readOnly かつ値が空だったら表示しない
  const enabledCompiler =
    enabledCompilerOptionRaw && (!readOnly || compilerOptionRaw.length !== 0);

  // 実行時オプションが有効であっても、readOnly かつ値が空だったら表示しない
  // 実行時オプションが無効であっても、明示的に実行時オプションを表示する設定が有効になっている場合は表示する
  const [showRuntime, setShowRuntime] = useState(false);
  const enabledRuntime = enabledRuntimeOptionRaw
    ? !readOnly || runtimeOptionRaw.length !== 0
    : showRuntime || runtimeOptionRaw.length !== 0;
  const showDropdown =
    enabledCompilerOptionRaw &&
    !enabledRuntimeOptionRaw &&
    !readOnly &&
    !showRuntime &&
    runtimeOptionRaw.length === 0;

  // runtimeOptionRaw に値が設定されたらずっと表示する
  useEffect(() => {
    if (runtimeOptionRaw.length === 0) {
      return;
    }
    setShowRuntime(true);
  }, [runtimeOptionRaw]);

  return (
    <div className="d-flex flex-column gap-16px">
      {enabledCompiler && (
        <div className="d-flex flex-column gap-8px">
          <div className="d-flex justify-content-between">
            <h6>{t("compiler.rawCompilerOptions")}</h6>
            {showDropdown && (
              <Dropdown className="wb-loadtemplate" align="end">
                <Dropdown.Toggle variant="link">
                  <ThreeDots />
                </Dropdown.Toggle>
                <Dropdown.Menu>
                  <Dropdown.Item onClick={() => setShowRuntime(true)}>
                    {t("compiler.showRawRuntimeOptions")}
                  </Dropdown.Item>
                </Dropdown.Menu>
              </Dropdown>
            )}
          </div>
          <div className="px-8px d-flex flex-column gap-4px">
            {readOnly ? (
              <div style={{ whiteSpace: "pre-wrap" }}>{compilerOptionRaw}</div>
            ) : (
              <Form.Control
                as="textarea"
                value={compilerOptionRaw}
                onChange={(e) => onChangeCompilerOptionRaw(e.target.value)}
              />
            )}
          </div>
        </div>
      )}
      {enabledRuntime && (
        <div className="d-flex flex-column gap-8px">
          <h6>{t("compiler.rawRuntimeOptions")}</h6>
          <div className="px-8px d-flex flex-column gap-4px">
            {readOnly ? (
              <div style={{ whiteSpace: "pre-wrap" }}>{runtimeOptionRaw}</div>
            ) : (
              <Form.Control
                as="textarea"
                value={runtimeOptionRaw}
                onChange={(e) => onChangeRuntimeOptionRaw(e.target.value)}
              />
            )}
          </div>
        </div>
      )}
    </div>
  );
};

export { RawCompilerOption };
