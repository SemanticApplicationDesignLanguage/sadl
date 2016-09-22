package com.ge.research.sadl.perspective.handlers;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.Issue;

import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.MessageManager.SadlMessage;
import com.ge.research.sadl.preferences.SadlPreferences;
import com.ge.research.sadl.processing.ValidationAcceptor;
import com.ge.research.sadl.ui.SadlConsole;
import com.ge.research.sadl.processing.IModelProcessor.ProcessorContext;
import com.ge.research.sadl.reasoner.IReasoner;

public class GraphImports extends SadlActionHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			String[] validTargetTypes = {"sadl","owl"};
			Object[] target = getCommandTarget(validTargetTypes);
			IProject project = null;
			IPath trgtFolder = null;
			IFile trgtFile = null;
			if (target != null) {
				if (target.length > 0) project = (IProject) target[0];
				if (target.length > 1) trgtFolder = (IPath) target[1];
				if (target.length > 2) trgtFile = (IFile) target[2];
			}
			String owlFileName = null;

			SadlConsole.writeToConsole(MessageType.INFO, "Graph of imports of '" + trgtFile.getName() + "' requested.\n");

			if (trgtFile.getName().endsWith("sadl")) {
				owlFileName = trgtFile.getFullPath().removeFileExtension().addFileExtension("owl").lastSegment();
			}
			else if (trgtFile.getName().endsWith("owl")) {
				owlFileName = trgtFile.getFullPath().lastSegment();
			}
			else {
				SadlConsole.writeToConsole(MessageType.ERROR, "Invalid selection for graphing imports");
				return event;
			}
			Map<String,String> prefMap = getPreferences();
			String renderClass = prefMap.get(SadlPreferences.GRAPH_RENDERER_CLASS.getId());
			if (renderClass == null || renderClass.length() == 0) {
//				List<IGraphVisualizer> visualizers = new ArrayList<IGraphVisualizer>();
//				ServiceLoader<IGraphVisualizer> serviceLoader = getReasonersFromServiceLoader(IGraphVisualizer.class);
//				if (serviceLoader != null) {
//					for (Iterator<IGraphVisualizer> itr = serviceLoader.iterator(); itr
//							.hasNext();) {
//						visualizers.add(itr.next());
//					}
//				}
				
			}
		}
		catch (Exception e) {
			SadlConsole.writeToConsole(MessageType.ERROR, e.getMessage() + "\n");
		}
		finally {
			
		}

		return event;
	}

//	protected static ServiceLoader<IGraphVisualizer> getVisualizersFromServiceLoader(
//			Class<IGraphVisualizer> cls) {
//		return ServiceLoader.load(cls);
//	}
}
