export interface ILanguage extends monaco.languages.ILanguageExtensionPoint {
    module?: ILanguageModule;
}

export interface ILanguageModule {
    conf: monaco.languages.LanguageConfiguration;
    language: monaco.languages.IMonarchLanguage;
}

const nullDisposable: monaco.IDisposable = {
    dispose() {
        // do nothing
    }
}

export function registerLanguage(language: ILanguage, loadLanguageConfiguration: boolean = true): monaco.IDisposable {
    monaco.languages.register(language);

    const module = language.module;
    if (!loadLanguageConfiguration || !module) {
        return nullDisposable;
    }

    const languageId = language.id;
    return monaco.languages.onLanguage(languageId, () => {
        monaco.languages.setMonarchTokensProvider(languageId, module.language);
        monaco.languages.setLanguageConfiguration(languageId, module.conf);
    });
}