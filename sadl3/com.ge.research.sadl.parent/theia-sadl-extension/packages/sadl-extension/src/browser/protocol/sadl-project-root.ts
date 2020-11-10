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

import { RequestType } from '@theia/languages/lib/browser';

export namespace SadlProjectRoot {

    export interface Params {
        readonly uri: string;
    }

    export interface Result {
        readonly uri: string | undefined;
    }

    export const TYPE = new RequestType<Params, Result, void, void>('sadl/projectRoot');

}
