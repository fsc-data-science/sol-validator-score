//!function(){var e=window.rudderanalytics=window.rudderanalytics||[];e.methods=["load","page","track","identify","alias","group","ready","reset","getAnonymousId","setAnonymousId"],e.factory=function(t){return function(){var r=Array.prototype.slice.call(arguments);return r.unshift(t),e.push(r),e}};for(var t=0;t<e.methods.length;t++){var r=e.methods[t];e[r]=e.factory(r)}e.loadJS=function(e,t){var r=document.createElement("script");r.type="text/javascript",r.async=!0,r.src="https://rsp-production.flipsidecrypto.workers.dev/dataPlane";var a=document.getElementsByTagName("script")[0];a.parentNode.insertBefore(r,a)},e.loadJS(),
//e.load('29a6Pl6fLlx4iYiOdfxRt0IBad9','https://rsp-production.flipsidecrypto.workers.dev'),
//e.page()}();

const writeKey = "29a6Pl6fLlx4iYiOdfxRt0IBad9";
const url = "https://rsp-production.flipsidecrypto.workers.dev";
rudderanalytics.load(writeKey, url, { configUrl: url, destSDKBaseURL: url + "/integrations" });
  