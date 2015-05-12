package com.ge.research.sadl.ui;

import org.eclipse.xtext.builder.impl.ToBeBuiltComputer;
import org.eclipse.xtext.builder.impl.XtextBuilder;
import org.eclipse.xtext.builder.impl.javasupport.JavaChangeQueueFiller;
import org.eclipse.xtext.builder.impl.javasupport.ProjectClasspathChangeListener;
import org.eclipse.xtext.builder.trace.JarEntryAwareTrace;
import org.eclipse.xtext.builder.trace.StorageAwareTrace;
import org.eclipse.xtext.generator.trace.DefaultTraceURIConverter;
import org.eclipse.xtext.ui.generator.trace.DefaultUITraceURIConverter;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.ui.resource.IStorage2UriMapperJdtExtensions;
import org.eclipse.xtext.ui.resource.Storage2UriMapperJavaImpl;
import org.eclipse.xtext.ui.resource.XtextResourceSetProvider;
import org.eclipse.xtext.ui.shared.internal.SharedModuleWithJdt;

public class SadlSharedModule extends SharedModuleWithJdt {
	@Override
	protected void configure() {
		bind(ToBeBuiltComputer.class).to(SadlToBeBuiltComputer.class);
		bind(ProjectClasspathChangeListener.class).asEagerSingleton();
		bind(IStorage2UriMapper.class).to(IStorage2UriMapperJdtExtensions.class);
		bind(IStorage2UriMapperJdtExtensions.class).to(Storage2UriMapperJavaImpl.class);
		bind(IResourceSetProvider.class).to(XtextResourceSetProvider.class);
//		bind(TypeResourceUnloader.class).asEagerSingleton();
		bind(JavaChangeQueueFiller.class).asEagerSingleton();
		bind(StorageAwareTrace.class).to(JarEntryAwareTrace.class);
		bind(DefaultTraceURIConverter.class).to(DefaultUITraceURIConverter.class);
	}
	
	

}
