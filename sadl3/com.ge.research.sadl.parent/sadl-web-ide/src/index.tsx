import './main.css';

// Import React and React DOM
import * as React from 'react';
import { render } from 'react-dom';

// Import the Hot Module Reloading App Container – more on why we use 'require' below
const { AppContainer } = require('react-hot-loader');

// Import our App container (which we will create in the next step)
import loadApp from './components/App';

// Tell Typescript that there is a global variable called module - see below
declare var module: { hot: any };

// Get the root element from the HTML
const appContainer = document.createElement('app');
document.body.appendChild(appContainer);

renderApplication(loadApp);


// Handle hot reloading requests from Webpack
if (module.hot) {
  module.hot.accept('./components/App', () => {
    // If we receive a HMR request for our App container, then reload it using require (we can't do this dynamically with import)
    const loadApp = require('./components/App').default;
    renderApplication(loadApp);
  })
}

function renderApplication(loadApp: () => Thenable<JSX.Element>): void {
  loadApp().then(app => {
    render(
      <AppContainer>
        {app}
      </AppContainer>,
      appContainer
    );
  });
}

