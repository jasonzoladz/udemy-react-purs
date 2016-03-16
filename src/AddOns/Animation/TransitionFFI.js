/* global exports */
"use strict";

// module AddOns.Animation.Transition
var React = require('react');
var ReactCSSTransitionGroup = require('react-addons-css-transition-group');

function unsafeFromPropsArray(props) {
  var result = {};

  for (var i = 0, len = props.length; i < len; i++) {
    var prop = props[i];

    for (var key in prop) {
      if (prop.hasOwnProperty(key)) {
        result[key] = prop[key];
      }
    }
  }

  return result;
};

exports.reactCSSTransitionGroup = function(props){
    var propsObject = unsafeFromPropsArray(props);

    return function(children){
      return React.createElement.apply(React, [ReactCSSTransitionGroup, propsObject].concat(children));
    };
};
