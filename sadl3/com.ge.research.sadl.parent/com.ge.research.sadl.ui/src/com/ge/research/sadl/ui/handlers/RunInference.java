package com.ge.research.sadl.ui.handlers;

import java.util.concurrent.TimeUnit;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.xtext.resource.XtextResource;

import com.ge.research.sadl.ide.handlers.SadlRunInferenceHandler;
import com.google.common.base.Supplier;
import com.google.common.base.Throwables;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class RunInference extends SadlActionHandler {
	
	@Inject
	private Provider<SadlRunInferenceHandler> handlerProvider;
	
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {	
			String[] validTargetTypes = getValidTargetFileTypes();
			Object[] target = getCommandTarget(validTargetTypes);
			IProject project = null;
			IPath trgtFolder = null;
			IFile trgtFile = null;
			if (target != null) {
				if (target.length > 0) project = (IProject) target[0];
				if (target.length > 1) trgtFolder = (IPath) target[1];
				if (target.length > 2) trgtFile = (IFile) target[2];
			}

			if (trgtFile != null) {
				generateInferenceJob(project, trgtFile);
			}
			else {
				console.error("No currently selected file; can't test model.\n");
			}
		}
		catch (Exception e) {
			console.error(e.getMessage() + "\n");
		}
		finally {
			
		}

		return event;
	}
	
	protected void generateInferenceJob(IProject project, IFile trgtFile) {
		Job inferenceJob = new Job("Inferencing") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				Thread inferenceThread;
				try {
					inferenceThread = new Thread() {
						public void run() {
							try {
								final XtextResource resource = getXtextResource(project, trgtFile);
								Supplier<XtextResource> resourceSupplier = () -> resource;
								final IProject currentProject = project;
								final IFile targetFile = trgtFile;
								SadlRunInferenceHandler delegate = handlerProvider.get();
								delegate.setGraphVisualizerHandler(new SadlEclipseGraphVisualizerHandler(RunInference.this, currentProject, targetFile));
								delegate.run(trgtFile.getLocation().toFile().toPath(), resourceSupplier);
								project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
							} catch (Exception e) {
								if (console != null) {
									console.error(Throwables.getStackTraceAsString(e));
								}
								else {
									System.err.println(e.getMessage());
								}
							}
						}
					};
					
					inferenceThread.start();
					
					TimeUnit.SECONDS.sleep(1);
					
					while (inferenceThread.isAlive()) {
						if (monitor.isCanceled()) {
							while (inferenceThread.isAlive()) {
								inferenceThread.stop();
//								inferenceThread.join();
							}
							console.info("Inference canceled.");
							return Status.CANCEL_STATUS;
						}
						
						monitor.worked(1);
						TimeUnit.SECONDS.sleep(1);
					}
				}
				catch (Exception e) {
//					return Status.CANCEL_STATUS;
				}
		    	return Status.OK_STATUS;
			}

		};
		inferenceJob.schedule();		
	}

	@Override
	protected String[] getValidTargetFileTypes() {
		String[] types = {"sadl","test"};
		return types;
	}

}