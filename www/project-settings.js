/**
* Stringmanager allows filename and projecttitle to be appended to the app's topbar
*/

function StringManager(settings){
    this.element = settings.element || null;
    this.separator = settings.separator || ' | ';
    this.projectString = settings.projectString;
    this.TargetString = settings.TargetString;
    this.ReferenceString = settings.ReferenceString;
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
    this.ReferenceString = string;
    this.updateElement();
};


StringManager.prototype.updateElement = function(){
    var strings = [];

    if(this.projectString) strings.push(this.projectString);
    if(this.TargetString) strings.push(this.TargetString);
    if(this.ReferenceString) strings.push(this.ReferenceString);

    this.element.textContent = strings.join(this.separator);
};

var setProjectButton = document.getElementById('crtPrj'),
	setNameButtonTarget = document.getElementById('setData.T'),
	setNameButtonReference = document.getElementById('setData.R'),
	projectContainer = document.createElement('span'),
	nav = document.querySelector('.navbar-static-top'),
    	titles = new StringManager({
		element: projectContainer,
		separator: ' | ',
		TargetString: 'Target: Default',
		ReferenceString: 'Reference: Default',
		projectString: 'Project: No project chosen'
	});

projectContainer.style.cssText = 'float: right; height: 50px; line-height: 50px; margin-right: 30px;';
projectContainer.innerHTML = 'Project: <span style="color:#b3dec1">No project chosen</span> | Target: <span style="color:#b3dec1">Default</span> | Reference: <span style="color:#b3dec1">Default</span>';


window.addEventListener('load',function(){
	var targetNode = document.getElementById('prjName'),
		config = { childList: true },
		observer = new MutationObserver(function(mutationsList, observer) {
		    for(var mutation of mutationsList) {
		        if (mutation.type == 'childList') {
		            titles.setProject('Project: '+ targetNode.textContent)
		        }
		    }
		});

	observer.observe(targetNode, config);

	setNameButtonTarget.addEventListener('click', function(){
		fileName = document.querySelector('[for="fileTarget"] + .input-group > input').value;
      titles.setTarget('Target: '+ fileName)
   })
	setNameButtonReference.addEventListener('click', function(){
		fileName = document.querySelector('[for="fileReference"] + .input-group > input').value;
      titles.setReference('Reference: '+ fileName)
   })
	
	nav.appendChild(projectContainer);

});


