import React from "react";
import ListGroup from "react-bootstrap/ListGroup";

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

  return (
    <ListGroup>
      <ListGroup.Item>Language</ListGroup.Item>
      {language === null ? (
        languages.map(
          (lang): React.ReactElement => {
            return (
              <ListGroup.Item
                key={lang}
                action
                onClick={(): void => onSelectLanguage(lang)}
              >
                {lang}
              </ListGroup.Item>
            );
          }
        )
      ) : (
        <ListGroup.Item>
          {language}
          {readOnly ? null : (
            <button
              type="button"
              className="close"
              aria-label="Close"
              onClick={onDeselectLanguage}
            >
              <span aria-hidden="true">&times;</span>
            </button>
          )}
        </ListGroup.Item>
      )}
    </ListGroup>
  );
};

export { ChooseLanguage };
