import i18n from "i18next";
import LanguageDetector from "i18next-browser-languagedetector";
import { initReactI18next } from "react-i18next";

const resources = {
  en: {
    translation: {
      ok: "OK",
      cancel: "Cancel",
      header: {
        settings: "Settings",
        history: "Log",
        language: "Language",
        languageEn: "English",
        languageJa: "日本語",
        github: "GitHub",
        login: "Login",
        logout: "Logout",
      },
      compiler: {
        language: "Language",
        compiler: "Compiler",
        loadTemplate: "Load template",
        options: "Options",
        rawCompilerOptions: "Raw compiler options",
        rawRuntimeOptions: "Raw runtime options",
      },
      title: {
        title: "Title",
        description: "Description",
      },
      author: {
        author: "Author",
        anonymous: "anonymous",
      },
      editor: {
        stdinTab: "Stdin",
      },
      settings: {
        title: "Editor Settings",
        tabKeyInserted: "TAB key inserted",
        tabKeyInserted2Spaces: "2 Spaces",
        tabKeyInserted4Spaces: "4 Spaces",
        tabKeyInserted8Spaces: "8 Spaces",
        tabKeyInsertedTab: "TAB",
        tabWidth: "TAB width",
        fixEditorHeight: "Fix editor height",
      },
      history: {
        title: "Ran/Viewed Log",
        ranAt: "Ran {{time}}",
        viewedAt: "Viewed {{time}}",
        createdAt: "Created at {{time}}",
        createdByUser: "Created by <a>@{{user}}</a>",
        createdByAnonymous: "Created by anonymous",
        view: "View",
        load: "Load",
      },
      run: {
        edit: "Clone & Edit",
        run: "Run",
        ctrlEnter: "or Ctrl+Enter",
      },
      result: {
        exitcode: "<d1>Exit Code: </d1><d2>{{exitCode}}</d2>",
        signal: "<d1>Signal: </d1><d2>{{signal}}</d2>",
      },
      permlink: {
        share: "Share",
      },
    },
  },
  ja: {
    translation: {
      ok: "OK",
      cancel: "キャンセル",
      header: {
        settings: "設定",
        history: "履歴",
        language: "言語",
        languageEn: "English",
        languageJa: "日本語",
        github: "GitHub",
        login: "ログイン",
        logout: "ログアウト",
      },
      compiler: {
        language: "言語",
        compiler: "コンパイラ",
        loadTemplate: "テンプレートのロード",
        options: "オプション",
        rawCompilerOptions: "コンパイル時オプション",
        rawRuntimeOptions: "実行時オプション",
      },
      title: {
        title: "タイトル",
        description: "詳細説明",
      },
      author: {
        author: "作成者",
        anonymous: "匿名ユーザ",
      },
      editor: {
        stdinTab: "標準入力",
      },
      settings: {
        title: "エディタ設定",
        tabKeyInserted: "タブキーで挿入されるテキスト",
        tabKeyInserted2Spaces: "2 スペース",
        tabKeyInserted4Spaces: "4 スペース",
        tabKeyInserted8Spaces: "8 スペース",
        tabKeyInsertedTab: "タブ",
        tabWidth: "タブ幅",
        fixEditorHeight: "エディタの高さを固定する",
      },
      history: {
        title: "実行/表示履歴",
        ranAt: "{{time}} に実行",
        viewedAt: "{{time}} に表示",
        createdAt: "{{time}} に作成",
        createdByUser: "<a>@{{user}}</a> が作成",
        createdByAnonymous: "匿名ユーザが作成",
        view: "表示",
        load: "ロード",
      },
      run: {
        edit: "コピーして編集",
        run: "実行",
        ctrlEnter: "または Ctrl+Enter",
      },
      result: {
        exitcode: "<d1>終了コード: </d1><d2>{{exitCode}}</d2>",
        signal: "<d1>シグナル: </d1><d2>{{signal}}</d2>",
      },
      permlink: {
        share: "共有",
      },
    },
  },
};

i18n
  .use(LanguageDetector)
  .use(initReactI18next)
  .init({
    resources: resources,

    fallbackLng: "en",
    debug: true,

    interpolation: {
      escapeValue: false,
    },
  });

export default i18n;
