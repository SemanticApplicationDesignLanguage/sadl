/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/

import { interfaces } from 'inversify';
import { createPreferenceProxy, PreferenceProxy, PreferenceService, PreferenceContribution, PreferenceSchema } from '@theia/core/lib/browser/preferences';

export const SadlConfigSchema: PreferenceSchema = {
    'type': 'object',
    properties: {
        'sadl.baseUri': {
            type: 'string',
            description: 'Base URI.',
            default: 'http://sadl.org'
        },
        'sadl.OWL_Format': {
            enum: ['RDF/XML-ABBREV', 'RDF/XML', 'N-TRIPLE', 'N3', 'Jena TDB'],
            description: 'Saved OWL model format.',
            default: 'RDF/XML'
        },
        'sadl.importBy': {
            enum: ['ns', 'fn'],
            description: 'Show import model list as either the model namespaces (ns) or thew SADL file names (fn).',
            default: 'ns'
        },
        'sadl.prefixesOnlyAsNeeded': {
            type: 'boolean',
            description: 'Show prefixes for imported concepts only when needed for disambiguation.',
            default: false
        },
        'sadl.validateBeforeTest': {
            type: 'boolean',
            description: 'Validate before testing.',
            default: false
        },
        'sadl.testWithKServer': {
            type: 'boolean',
            description: 'Test/Query with knowledge server.',
            default: false
        },
        'sadl.namespacesInQueryResults': {
            type: 'boolean',
            description: 'Show Namespaces in Query Results',
            default: false
        },
        'sadl.showTimingInformation': {
            type: 'boolean',
            description: 'Show timing information for the build and the reasoning.',
            default: false
        },
        'sadl.dmyOrder': {
            enum: ['mdy', 'dmy'],
            description: 'Interpret date 10/11/2012 as MM/DD/YYYY (mdy) or DD/MM/YYYY (dmy).',
            default: 'mdy'
        },
        'sadl.deepValidationOff': {
            type: 'boolean',
            description: 'Disable the deep validation of the model.',
            default: false
        },
        'sadl.graphrendererclass': {
            type: 'string',
            description: 'Graph renderer package and class.'
        },
        'sadl.graphImplicitElements': {
            type: 'boolean',
            description: 'Include implicit elements in the graph.',
            default: false
        },
        'sadl.graphImplicitElementInstances': {
            type: 'boolean',
            description: 'Include implicit element instances in the graph.',
            default: false
        },
        'sadl.ambiguousNameCheckOn': {
            type: 'boolean',
            description: 'Check for ambiguous names during the validation.',
            default: true
        },
        'sadl.cardinalityCheck': {
            type: 'boolean',
            description: 'Check for cardinality of property on specific domain.',
            default: false
        },
        'sadl.useArticlesInValidation': {
            type: 'boolean',
            description: 'Use indefinite and definite articles in validation and translation.',
            default: false
        },
        'sadl.typeCheckingWarningOnly': {
            type: 'boolean',
            description: 'Type checking issues as warning only.',
            default: false
        },
        'sadl.ignoreUnittedQuantities': {
            type: 'boolean',
            description: 'Ignore Unitted Quantities (treat as numeric only) during translation.',
            default: false
        },
        'sadl.domainAndRangeAsUnionClasses': {
            type: 'boolean',
            description: 'Translate multiple-class domain or range as union class (owl:unionOf).',
            default: true
        },
        // TODO: does it make any sense here? Shall we need a `Full Clean/Build` operation then?
        'sadl.generateMetricsReport': {
            type: 'boolean',
            description: 'Generate metrics report during project clean/build.',
            default: false
        },
        // TODO: this should be relative for sure but relative to what: workspace or project?
        'sadl.metricsQueryFilename': {
            type: 'string',
            description: 'File containing metric queries. This file should be relative to the current preference file and the relative path must contain only forward slashes.'
        }
    }
};

export namespace DefaultSadlConfigSchema {
    export function content(): string {
        const entries: string[] = [];
        for (const key in SadlConfigSchema.properties) {
            const value = SadlConfigSchema.properties[key];
            if (value && value.default !== undefined) {
                const { type, description } = value;
                const valueEscape = type === 'boolean' || type === 'null' || type === 'number' ? '' : '"';
                entries.push(`     // ${description}
    "${key}": ${valueEscape}${value.default}${valueEscape}`);
            }
        }
        return `{
${entries.join(',\n')}
}`;
    }
}

export interface SadlConfiguration {
    'sadl.baseURI': string;
    'sadl.owlModelFormat': 'RDF/XML-ABBREV' | 'RDF/XML' | 'N-TRIPLE' | 'N3' | 'Jena TDB';
    'sadl.importBy': 'ns' | 'fn';
    'sadl.prefixOnlyAsNeeded': boolean;
    'sadl.validateBeforeTest': boolean;
    'sadl.testWithKnowledgeServer': boolean;
    'sadl.namespaceInQueryResults': boolean;
    'sadl.showTimingInformation': boolean;
    'sadl.dmyOrder': 'mdy' | 'dmy';
    'sadl.deepValidationOff': boolean;
    'sadl.graph.rendererClass'?: string;
    'sadl.graph.implicitElements': boolean;
    'sadl.graph.implicitElementInstances': boolean;
    'sadl.checkForAmbiguousNames': boolean;
    'sadl.checkForCardinalityOfPropertyInDomain': boolean;
    'sadl.useArticlesInValidation': boolean;
    'sadl.typeCheckingWarningOnly': boolean;
    'sadl.ignoreUnittedQuantities': boolean;
    'sadl.createDomainAndRangesAsUnionClasses': boolean;
    'sadl.generateMetricsOnCleanBuild': boolean;
    'sadl.metricsQueryFile'?: string;
}

export const SadlPreferences = Symbol('SadlPreferences');
export type SadlPreferences = PreferenceProxy<SadlConfiguration>;

export function createSadlPreferences(preferences: PreferenceService): SadlPreferences {
    return createPreferenceProxy(preferences, SadlConfigSchema);
}

export function bindSadlPreferences(bind: interfaces.Bind): void {
    bind(SadlPreferences).toDynamicValue(ctx => {
        const preferences = ctx.container.get<PreferenceService>(PreferenceService);
        return createSadlPreferences(preferences);
    });
    bind(PreferenceContribution).toConstantValue({ schema: SadlConfigSchema });
}