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

import com.ge.research.sadl.SADLRuntimeModule;
import com.ge.research.sadl.builder.MessageManager;
import com.ge.research.sadl.ide.handlers.SadlGraphVisualizerHandler;
import com.ge.research.sadl.utils.SadlIdeProjectHelper;
import com.ge.research.sadl.model.visualizer.GraphVizVisualizer;
import com.ge.research.sadl.model.visualizer.IGraphVisualizer;
import com.ge.research.sadl.reasoner.ResultSet;
import com.ge.research.sadl.utils.EclipseSadlProjectHelper;
import com.ge.research.sadl.utils.SadlConsole;
import com.ge.research.sadl.utils.SadlProjectHelper;
import com.google.inject.ConfigurationException;
import com.google.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.util.Map;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.builder.clustering.CurrentDescriptions;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.containers.IAllContainersState.Provider;
import org.eclipse.xtext.resource.containers.ResourceSetBasedAllContainersStateProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Customized SADL runtime module that contains a workaround for the
 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=426212 issue.
 */
@SuppressWarnings("restriction")
public class SADLCliAppRuntimeModule extends SADLRuntimeModule {

    @Override
    public Class<? extends Provider> bindIAllContainersState$Provider() {
        return MyResourceSetBasedAllContainersStateProvider
                .class; // org.eclipse.xtext.ui.containers.ContainerStateProvider
    }

    public static class MyResourceSetBasedAllContainersStateProvider
            extends ResourceSetBasedAllContainersStateProvider implements Provider {

        @Override
        protected ResourceSet getResourceSet(IResourceDescriptions context) {
            if (context instanceof CurrentDescriptions) {
                Notifier target = ((CurrentDescriptions) context).getTarget();
                if (target instanceof ResourceSet) {
                    return (ResourceSet) target;
                }
            }
            return super.getResourceSet(context);
        }
    }

    public Class<? extends SadlConsole> bindSadlConsole() {
        return MySadlConsole.class;
    }

    public static class MySadlConsole implements SadlConsole {
        private static final Logger LOGGER = LoggerFactory.getLogger(MySadlConsole.class);

        @Override
        public void print(final MessageManager.MessageType type, final String message) {
            switch (type) {
                case ERROR:
                    LOGGER.error(message);
                    break;
                case WARN:
                    LOGGER.warn(message);
                    break;
                case INFO:
                    LOGGER.info(message);
                    break;
                default:
                    LOGGER.debug(message);
                    break;
            }
        }
    }

    public Class<? extends SadlProjectHelper> bindSadlProjectHelper() {
        return MySadlProjectHelper.class;
    }

    public static class MySadlProjectHelper implements SadlProjectHelper {
        private EclipseSadlProjectHelper eclipseHelper = new EclipseSadlProjectHelper();
        private SadlIdeProjectHelper ideHelper = new SadlIdeProjectHelper();

        private boolean usesFileScheme(final URI uri) {
            return "file".equalsIgnoreCase(uri.getScheme());
        }

        @Override
        public boolean isRoot(final URI uri) {
            if (usesFileScheme(uri)) {
                return ideHelper.isRoot(uri);
            } else {
                return eclipseHelper.isRoot(uri);
            }
        }

        @Override
        public URI getRoot(final URI nested) {
            if (usesFileScheme(nested)) {
                return ideHelper.getRoot(nested);
            } else {
                return eclipseHelper.getRoot(nested);
            }
        }

        @Override
        public Path toPath(final URI uri) {
            if (usesFileScheme(uri)) {
                return ideHelper.toPath(uri);
            } else {
                return eclipseHelper.toPath(uri);
            }
        }
    }

    public Class<? extends SadlGraphVisualizerHandler> bindSadlGraphVisualizerHandler() {
        return MySadlGraphVisualizerHandler.class;
    }

    public static class MySadlGraphVisualizerHandler implements SadlGraphVisualizerHandler {
        @Inject private SadlProjectHelper projectHelper;
        private IGraphVisualizer visualizer = new GraphVizVisualizer();

        @Override
        public void resultSetToGraph(
                final Path path,
                final ResultSet resultSet,
                final String description,
                final String baseFileName,
                final IGraphVisualizer.Orientation orientation,
                final Map<String, String> properties)
                throws ConfigurationException, IOException {
            File projectDirectory = new File(projectHelper.getRoot(path.toUri()));
            File graphsDirectory = new File(projectDirectory, "Graphs");
            graphsDirectory.mkdirs();
            visualizer.initialize(
                    graphsDirectory.getAbsolutePath(),
                    baseFileName,
                    baseFileName,
                    null,
                    orientation,
                    description);
            visualizer.graphResultSetData(resultSet);
        }
    }
}
