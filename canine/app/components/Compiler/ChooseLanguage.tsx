import React from "react";
import ListGroup from "react-bootstrap/ListGroup";
import { useTranslation } from "react-i18next";

interface ChooseLanguageProps {
  language: string | null;
  languages: string[];
  readOnly: boolean;
  onSelectLanguage: (language: string) => void;
  onDeselectLanguage: () => void;
}

const ChooseLanguage: React.FC<ChooseLanguageProps> = (
  props
): React.ReactElement => {
  const {
    language,
    languages,
    readOnly,
    onSelectLanguage,
    onDeselectLanguage,
  } = props;
  const { t } = useTranslation();

  return language === null ? (
    <ListGroup className="wb-languagelist">
      <ListGroup.Item>{t("compiler.language")}</ListGroup.Item>
      {languages.map((lang): React.ReactElement => {
        return (
          <ListGroup.Item
            key={lang}
            action
            onClick={(): void => onSelectLanguage(lang)}
          >
            {lang}
          </ListGroup.Item>
        );
      })}
    </ListGroup>
  ) : (
    <div className="d-flex flex-column gap-8px wb-languagelist-selected">
      <h6>{t("compiler.language")}</h6>
      <div className="px-8px d-flex justify-content-between">
        <p>{language}</p>
        {readOnly ? null : (
          <button
            type="button"
            className="btn-close"
            aria-label="Close"
            onClick={onDeselectLanguage}
          />
        )}
      </div>
    </div>
  );
};

export { ChooseLanguage };
