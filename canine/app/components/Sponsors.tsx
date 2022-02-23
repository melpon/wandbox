import React from "react";
import { useTranslation } from "react-i18next";

import { SponsorsGetData } from "~/hooks/sponsors";

interface SponsorsProps {
  sponsors: SponsorsGetData;
}

const Sponsors: React.FC<SponsorsProps> = (props) => {
  const { t } = useTranslation();
  const { sponsors } = props;

  return (
    // ここの表示は CSS で頑張る
    <div className="wb-sponsors d-flex flex-column gap-8px">
      <div>
        <label>{t("sponsors.corporate")}</label>
        <div>
          {sponsors.corporate.map((x) => {
            if (x.url.length !== 0) {
              return (
                <a
                  key={x.name}
                  target="_blank"
                  rel="noopener noreferrer"
                  href={x.url}
                >
                  {x.name}
                </a>
              );
            } else {
              return <p key={x.name}>{x.name}</p>;
            }
          })}
        </div>
      </div>
      <div>
        <label>{t("sponsors.personal")}</label>
        <div>
          {sponsors.personal.map((x) => {
            if (x.url.length !== 0) {
              return (
                <a
                  key={x.name}
                  target="_blank"
                  rel="noopener noreferrer"
                  href={x.url}
                >
                  {x.name}
                </a>
              );
            } else {
              return <p key={x.name}>{x.name}</p>;
            }
          })}
        </div>
      </div>
    </div>
  );
};

export { Sponsors };
