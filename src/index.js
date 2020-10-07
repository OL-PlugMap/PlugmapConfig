import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var elm = Elm.Main.init({
  node: document.getElementById('root')
});

elm.ports.logError.subscribe((errors) => { errors.forEach(element => {
  console.log(element)
})})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
