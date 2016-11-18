import {
    ILanguage, registerLanguage
} from '../monaco/languages';

export interface LanguageDescription {
    languageId: string
    aliases: string[]
    mimeTypes: string[]
    fileExtensions: string[]
    highlightingConfiguration: string
    module?: any
}

export function toMonacoLanguage(language: LanguageDescription): ILanguage {
    const extensions = language.fileExtensions.map((it) => {
        return '.' + it
    });
    return {
        id: language.languageId,
        extensions: extensions,
        aliases: language.aliases,
        mimetypes: language.mimeTypes,
        configuration: language.highlightingConfiguration,
        module: language.module
    };
}

export function registerLanguages(supportedLanguages: LanguageDescription[]) {
    for (const language of supportedLanguages) {
        registerLanguage(toMonacoLanguage(language));
    }
}