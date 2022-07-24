import React, { useCallback, useEffect, useMemo, useState } from "react";

import { useError } from "~/hooks/error";
import i18n from "~/i18n";
import { useUserPermlinkList } from "~/hooks/userPermlinkList";
import { Nav, Navbar, NavDropdown } from "react-bootstrap";
import { useTranslation } from "react-i18next";
import { CodeMirror6, CodeMirror6Option } from "./CodeMirror6";
import { Extension } from "@codemirror/state";
import { importLanguage, resolveLanguage } from "~/utils/resolveLanguageMode";
import { loadSettings } from "~/features/actions";
import { EditorView } from "@codemirror/view";

interface UserHeaderProps {}

const UserHeader: React.FC<UserHeaderProps> = () => {
  const { t } = useTranslation();
  const { githubUser } = WANDBOX_LOADER_DATA;

  return (
    <Navbar
      className="px-16px justify-content-between"
      bg="dark"
      variant="dark"
      style={{ padding: "5px 16px", zIndex: 10 }}
      expand="md"
    >
      <Navbar.Brand href="/">Wandbox</Navbar.Brand>
      <Navbar.Collapse id="wb-navbar" className="justify-content-end">
        <Nav className="align-items-md-center">
          <NavDropdown title={t("header.language")} align="end">
            <NavDropdown.Item
              onClick={() => {
                i18n.changeLanguage("en-US");
              }}
            >
              {t("header.languageEn")}
            </NavDropdown.Item>
            <NavDropdown.Item
              onClick={() => {
                i18n.changeLanguage("ja-jp");
              }}
            >
              {t("header.languageJa")}
            </NavDropdown.Item>
          </NavDropdown>
          <Nav.Link
            target="_blank"
            rel="noopener noreferrer"
            href="https://github.com/melpon/wandbox"
          >
            {t("header.github")}
          </Nav.Link>
          {githubUser === null ? (
            <Nav.Link
              href={`https://github.com/login/oauth/authorize?client_id=${WANDBOX_GITHUB_CLIENT_ID}`}
            >
              {t("header.login")}
            </Nav.Link>
          ) : (
            <NavDropdown
              className="wb-nav-icondropdown"
              title={
                <img
                  src={githubUser.avatar_url}
                  style={{ width: 24, height: 24, borderRadius: 12 }}
                />
              }
              align="end"
            >
              <NavDropdown.Item href="/logout">
                {t("header.logout")}
              </NavDropdown.Item>
            </NavDropdown>
          )}
        </Nav>
      </Navbar.Collapse>
    </Navbar>
  );
};

interface ContentProps {
  title: string;
  description: string;
  code: string;
  language: string;
  createdAt: number;
  permlinkId: string;
  tabWidth: string;
}
const Content: React.FC<ContentProps> = (props) => {
  const {
    title,
    description,
    code,
    language,
    createdAt,
    permlinkId,
    tabWidth,
  } = props;

  const [languageSupport, setLanguageSupport] = useState<Extension | null>(
    null
  );
  const resolvedLanguage = resolveLanguage(null, language);
  useEffect((): void => {
    importLanguage(resolvedLanguage).then((lang) => {
      setLanguageSupport(lang);
    });
  }, [resolvedLanguage]);

  const option = useMemo((): CodeMirror6Option => {
    return {
      lineNumbers: true,
      mode: "default",
      tabSize: parseInt(tabWidth, 10),
      languageSupport: languageSupport || undefined,
      readOnly: true,
      keymaps: [],
    };
  }, [tabWidth, languageSupport]);

  const [view, setView] = useState<EditorView | undefined>(undefined);

  return (
    <div className="d-flex flex-column gap-8px">
      <h4 className="text-info" style={{ wordBreak: "break-word" }}>
        {title}
      </h4>
      <p style={{ whiteSpace: "pre-wrap", wordBreak: "break-word" }}>
        {description}
      </p>
      <CodeMirror6
        className="wb-listcontent-cm"
        text={code}
        view={view}
        option={option}
        onViewCreated={(view) => {
          setView(view);
        }}
        onViewDestroyed={() => {
          setView(undefined);
        }}
        onChange={() => {}}
      />
      <p>{createdAt}</p>
      <a href={`/permlink/${permlinkId}`}>{permlinkId}</a>
    </div>
  );
};

const User: React.FC = () => {
  i18n;

  const [initialized, setInitialized] = useState(false);
  const [userError, setUserError] = useState<null | string>(null);
  const [userPermlinkList, , doUserPermlinkList] = useUserPermlinkList(
    "",
    setUserError
  );

  const [tabWidth, setTabWidth] = useState<string>("4");

  useEffect((): void => {
    const settings = loadSettings();
    setTabWidth(settings.editorSettings.tabWidth);
  }, []);

  useEffect(() => {
    if (!initialized) {
      return;
    }

    doUserPermlinkList(`/api/user/permlink${document.location.search}`, {});
  }, [initialized]);

  useEffect(() => {
    setInitialized(true);
  }, []);
  if (!initialized) {
    return null;
  }

  return (
    <div id="wb-main" className="d-flex flex-column">
      <UserHeader />
      <div className="d-flex flex-column px-32px py-24px gap-24px">
        {userError !== null && <p>ログインして下さい</p>}
        {userPermlinkList &&
          userPermlinkList.rows.map((row) => {
            return (
              <Content
                title={row.parameter.title}
                description={row.parameter.description}
                code={row.parameter.code}
                language={row.parameter.compilerInfo.language}
                createdAt={row.parameter.createdAt}
                permlinkId={row.permlinkId}
                tabWidth={tabWidth}
              />
            );
          })}
      </div>
    </div>
  );
};

export { User };
