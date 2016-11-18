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

export function findLanguageIdByURI(uri: string): string | null {
    const language = findLanguageByURI(uri);
    return language ? language.id : null;
}

export function findLanguageByURI(uri: string): monaco.languages.ILanguageExtensionPoint | null {
    const extension = getExtension(uri);
    if (extension) {
        return findLanguageByExtension(uri);
    }
    return null;
}

export function findLanguageByExtension(extension: string): monaco.languages.ILanguageExtensionPoint | null {
    for (const language of monaco.languages.getLanguages()) {
        if (language.extensions && language.extensions.indexOf(extension) !== -1) {
            return language;
        }
    }
    return null;
}

export function getExtension(uri: string): string | null {
    const index = uri.indexOf('.');
    return index === -1 ? null : uri.substr(index + 1);
}
