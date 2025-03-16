import React, { useState } from "react";
import { Dropdown } from "react-bootstrap";
import { ThreeDots } from "react-bootstrap-icons";
import ListGroup from "react-bootstrap/ListGroup";
import { useTranslation } from "react-i18next";
import { MergedHppInfo } from "~/features/slice";

import { CompilerInfo } from "~/hooks/compilerList";
import { HpplibDialog } from "~/components/Compiler/HpplibDialog";

interface ChooseCompilerProps {
  compilerInfo: CompilerInfo | null;
  compilerInfos: CompilerInfo[];
  readOnly: boolean;
  templates: string[];
  hpplib: MergedHppInfo[];
  onSelectCompiler: (compiler: CompilerInfo) => void;
  onDeselectCompiler: () => void;
  onLoadTemplate: (template: string) => void;
}

const ChooseCompiler: React.FC<ChooseCompilerProps> = (
  props
): React.ReactElement => {
  const {
    compilerInfo,
    compilerInfos,
    readOnly,
    templates,
    hpplib,
    onSelectCompiler,
    onDeselectCompiler,
    onLoadTemplate,
  } = props;
  const { t } = useTranslation();

  const [showHpplibDialog, setShowHpplibDialog] = useState<boolean>(false);

  return compilerInfo === null ? (
    <ListGroup className="wb-compilerlist">
      <ListGroup.Item>{t("compiler.compiler")}</ListGroup.Item>
      {compilerInfos.map((info): React.ReactElement => {
        return (
          <ListGroup.Item
            className="wb-item"
            key={info.name}
            action
            onClick={(): void => onSelectCompiler(info)}
          >
            {`${info.displayName} ${info.version}`}
          </ListGroup.Item>
        );
      })}
    </ListGroup>
  ) : (
    <div className="d-flex flex-column gap-8px wb-compilerlist-selected">
      <div className="d-flex justify-content-between">
        <h6>{t("compiler.compiler")}</h6>
        <HpplibDialog show={showHpplibDialog} hpplib={hpplib} onHide={() => setShowHpplibDialog(false)} />
        {!readOnly && (
          <Dropdown className="wb-loadtemplate" align="end">
            <Dropdown.Toggle variant="link">
              <ThreeDots />
            </Dropdown.Toggle>
            <Dropdown.Menu>
              {templates.map((template) => {
                return (
                  <Dropdown.Item
                    key={template}
                    onClick={() => onLoadTemplate(template)}
                  >
                    {t("compiler.loadTemplate")}
                  </Dropdown.Item>
                );
              })}
              {compilerInfo.language === "C++" && (
                <Dropdown.Item
                  key="show-hpp"
                  onClick={() => setShowHpplibDialog(true)}
                >
                  {t("compiler.showHpplibDialog")}
                </Dropdown.Item>
              )}
            </Dropdown.Menu>
          </Dropdown>
        )}
      </div>
      <div className="px-8px d-flex justify-content-between">
        <p>{`${compilerInfo.displayName} ${compilerInfo.version}`}</p>
        {readOnly ? null : (
          <button
            type="button"
            className="btn-close"
            aria-label="Close"
            onClick={onDeselectCompiler}
          />
        )}
      </div>
    </div>
  );
};

export { ChooseCompiler };
