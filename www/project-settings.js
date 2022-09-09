/**
* Stringmanager allows filename and projecttitle to be appended to the app's topbar
*/

function StringManager(settings){
    this.element = settings.element || null;
    this.separator = settings.separator || ' | ';
    this.projectString = settings.projectString;
    this.TargetString = settings.TargetString;
    this.ReferenceString = settings.ReferenceString;
    this.useDataClicked = false;
}

StringManager.prototype.setProject = function(string){
    this.projectString = string;
    this.updateElement();
};

StringManager.prototype.setTarget = function(string){
    this.TargetString = string;
    this.updateElement();
};

StringManager.prototype.setReference = function(string){
    if(!this.useDataClicked) return;
    
    this.ReferenceString = string;
    this.updateElement();
};


StringManager.prototype.updateElement = function(){
    var strings = [];

    if(this.projectString) strings.push(this.projectString);
    if(this.TargetString) strings.push(this.TargetString);
    if(this.ReferenceString) strings.push(this.ReferenceString);

    this.element.innerHTML = strings.join(this.separator);
};

var setProjectButton = document.getElementById('crtPrj'),
	setNameButtonTarget = document.getElementById('setData.T'),
	setNameButtonReference = document.getElementById('setData.R'),
	projectContainer = document.createElement('span'),
	nav = document.querySelector('.navbar-static-top'),
  titles = new StringManager({
		element: projectContainer,
		separator: ' | ',
		TargetString: 'Target: <span style="color:#ccffff">None</span>',
		ReferenceString: 'Reference: <span style="color:#ccffff">None</span>',
		projectString: 'Project: <span style="color:#ccffff">None</span>'
	});

projectContainer.style.cssText = 'float: right; height: 50px; line-height: 50px; margin-right: 30px;';
titles.updateElement();

window.addEventListener('load',function(){
	var prjNode = document.getElementById('prjName'),
		
	prjObserver = new MutationObserver(function(mutationsList) {
	    for(var mutation of mutationsList) {
	        if (mutation.type == 'childList') {
	            titles.setProject('Project: <span style="color:#ccffff">'+ prjNode.textContent + ' </span>')
	        }
	    }
	});

	prjObserver.observe(prjNode, { childList: true });
	
	nav.appendChild(projectContainer);
});

window.addEventListener("click",function(e){
  if(!e.target.closest('[id="setData.R"]')) return;
  
  titles.useDataClicked = true;
  var node = document.getElementById('ReferenceName');
  titles.setReference('Reference: <span style="color:#ccffff">'+ node.textContent + ' </span>')
})
    
window.addEventListener("click",function(e){
  if(!e.target.closest('[id="setData.T"]')) return;
  
  titles.useDataClicked = true;
  var node = document.getElementById('TargetName');
  titles.setTarget('Target: <span style="color:#ccffff">'+ node.textContent + ' </span>')
})