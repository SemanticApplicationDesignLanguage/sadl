package org.example.domainmodel.ide.lsp.extension;

import java.util.concurrent.ExecutorService;

import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.xtext.ide.ExecutorServiceProvider;
import org.eclipse.xtext.ide.server.DefaultProjectDescriptionFactory;
import org.eclipse.xtext.ide.server.IMultiRootWorkspaceConfigFactory;
import org.eclipse.xtext.ide.server.IProjectDescriptionFactory;
import org.eclipse.xtext.ide.server.MultiRootWorkspaceConfigFactory;
import org.eclipse.xtext.ide.server.ServerModule;
import org.eclipse.xtext.resource.IContainer;
import org.eclipse.xtext.resource.IResourceServiceProvider;
import org.eclipse.xtext.resource.ResourceServiceProviderServiceLoader;
import org.eclipse.xtext.resource.containers.ProjectDescriptionBasedContainerManager;

public class DomainmodelServerModule extends ServerModule {

	@Override
	protected void configure() {
		binder().bind(ExecutorService.class).toProvider(ExecutorServiceProvider.class);

		bind(LanguageServer.class).to(DomainmodelLanguageServer.class);
		bind(IResourceServiceProvider.Registry.class).toProvider(ResourceServiceProviderServiceLoader.class);
		bind(IMultiRootWorkspaceConfigFactory.class).to(MultiRootWorkspaceConfigFactory.class);
		bind(IProjectDescriptionFactory.class).to(DefaultProjectDescriptionFactory.class);
		bind(IContainer.Manager.class).to(ProjectDescriptionBasedContainerManager.class);
	}

}
