/**
 * Copyright © 2007-2018 - General Electric Company, All Rights Reserved
 *
 * <p>Project: SADL
 *
 * <p>Description: The Semantic Application Design Language (SADL) is a language for building
 * semantic models and expressing rules that capture additional domain knowledge. The SADL-IDE
 * (integrated development environment) is a set of Eclipse plug-ins that support the editing and
 * testing of semantic models using the SADL language.
 *
 * <p>This software is distributed "AS-IS" without ANY WARRANTIES and licensed under the Eclipse
 * Public License - v 1.0 which is available at http://www.eclipse.org/org/documents/epl-v10.php
 */
package com.ge.research.sadl.applications;

import com.ge.research.sadl.SADLStandaloneSetup;
import com.google.inject.Guice;
import com.google.inject.Injector;

/**
 * Standalone setup for the headless CLI application.
 *
 * <p>This is a workaround for https://github.com/crapo/sadlos2/issues/353#issuecomment-448272712.
 * We cannot use standalone setup when an Eclipse platform is up an running.
 */
public class SADLCliAppStandaloneSetup extends SADLStandaloneSetup {

    @Override
    public Injector createInjector() {
        return Guice.createInjector(new SADLCliAppRuntimeModule());
    }
}
