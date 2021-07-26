# Call for Code: Clean water and sanitation

Use this starter kit to understand how technology can improve access to clean drinking water, reduce water waste, and protect natural resources. 
The starter kit provides tools and resources from our experts to help you jump-start your own solution for the 2021 Call for Code Global Challenge.

Mira nuestra [solución forest4water](http://169.60.171.142:3838/call4code/)

## Descripción 📄

### Pre-requisitos 📋  
Un Virtual Server for Classic IBM con entorno Ubuntu (20.04.2 LTS)

### Instalación 🔧
Lanzar la máquina virtual. Instalar R y Shinny-server usando el código install-r-shiny-server dentro del folder backend
Instalar docker, docker-compose usando el archivo bash.sh
Mover  los archivos api.R, main.R y demás de la carpeta backend en la caprta ~/plumber/
Desplegar el API usando el comando desde ~/pluber/: sudo docker stack deploy -c docker-compose.yml api # init services

_Para instalar el chatboot de watson assistant_
* [Abrir Watson Asistant](https://cloud.ibm.com/docs/assistant?topic=assistant-getting-started#getting-started-launch-tool)
* [Crear un asistente](https://cloud.ibm.com/docs/assistant?topic=assistant-getting-started#getting-started-create-assistant)
* [Crear un conocimiento de diálogo](https://cloud.ibm.com/docs/assistant?topic=assistant-getting-started#getting-started-add-skill)
* Importar habilidad y cargar skill-Asistente-guiado.json
* [Integrar el chatbot a la web](https://cloud.ibm.com/docs/assistant?topic=assistant-deploy-web-chat)

### Ejecutando las pruebas ⚙️
Abrir R desde consola linux: R
Acceder al servidor shiny acccediendo a la página: http://169.60.171.142:3838

### Despliegue 📦
Copiar los archivos de la carpeta /frontend al folder /srv/shiny-server/call4code 
Acceder a la página http://169.60.171.142:3838/call4code

### Link de Youtube
Nuestro Canal de Youtube [Forest4Water](https://www.youtube.com/channel/UCvzjHuCK_IibzNNFpnNbBYQ/featured)

### Construido con 🛠️
IBM Cloud, R, Docker, HTML, GoogleEarthEngine

## Autores ✒️

<div>
  <div>
  <img href="https://github.com/gonzalezivan90" align="left" src="https://avatars.githubusercontent.com/u/5403068?v=4" height="50" width="50"> 
    <!--
    <aside>
      <h5>gonzalezivan90</h5>
      <p>Hola, soy...</p>
    </aside>
    --> 
  <img href="https://github.com/danflop" align="left" src="https://avatars.githubusercontent.com/u/5290060?v=4" height="50" width="50">     
  <img href="https://github.com/neo-zero98" align="left" src="https://avatars.githubusercontent.com/u/74437268?v=4" height="50" width="50">  
  <img href="https://github.com/elvisdev0" align="left" src="https://avatars.githubusercontent.com/u/57382598?v=4" height="50" width="50">  
  <img href="https://github.com/ferjml97" align="left" src="https://avatars.githubusercontent.com/u/47682546?v=4" style="border-radius: 100px" height="50" width="50">
  </div>
</div>
