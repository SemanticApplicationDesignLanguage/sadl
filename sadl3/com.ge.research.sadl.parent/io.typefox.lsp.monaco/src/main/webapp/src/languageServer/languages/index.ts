import {
    LanguageDescription
} from '../protocol/registry';

import {
    ILanguage
} from '../../monaco/languages';

export const supportedLanguages: LanguageDescription[] = [
    {
        languageId: 'com.ge.research.sadl.SADL',
        aliases: ['sadl'],
        mimeTypes: [],
        fileExtensions: ["sadl"],
        highlightingConfiguration: ''
    }
];

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