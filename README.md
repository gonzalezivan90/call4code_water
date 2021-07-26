# Call for Code: Clean water and sanitation

Use this starter kit to understand how technology can improve access to clean drinking water, reduce water waste, and protect natural resources. 
The starter kit provides tools and resources from our experts to help you jump-start your own solution for the 2021 Call for Code Global Challenge.

Prueba nuestra solución [forest4water!](http://169.60.171.142:3838/call4code/). Escríbenos tus comentarios o dudas :)

## Descripción 📄
Esta herramienta pretende resolver el problema del Call4Code para el reto de agua limpia. Nosotros creemos que:
- El agua en rios y lagunas depende mayoritariamente de la lluvia. 
- Los humanos no podemos hacer que llueva más o menos
- Las actividades humanas, como la deforestación, están incrementando la variablilidad y eventos extremos de las lluvias: más sequías, más inundaciones.
- Los bosques, que hacen no llover, sí regulan la cantidad del agua: Atenúan el impacto de las sequías liberando agua acumulada, y disminuyen la severidad de inundaciones como resistencia mecánica y absorbiendo parte del agua
- La calidad del agua depende, entre otros, de la cantidad: en sequías y en avalanchas, los sedimentos en el  agua son elevados
- Para tener agua potable, necesitamos que el caudal no tenga ni mucha no poca agua, pero tambien que no hayan sedimentos
- Los bosques tambien ayudan a retener sedimentos, pues cuando no hay vegetación, la lluvia y el aire aportan particulas a los cuerrpos de agua
- Mantener los bosques ayuda a regular cantidad y calidad de agua, en lo que se conoce como servicio ecosistémico de regulación hídrica

La herramienta permite al usuario lo siguiente:
- Basdado en un click, identificar cual es mi río más cercano, así como la cuenca en la está, y el punto más cercano al que debo desplazarme para encontrar agua. Esto soluciona un problema inmediato de acceso al agua.
- Identificar la cuenca en la que estamos, pues el agua que veamos en el río depende de condiciones regionales.
- Identificando la cuenca, extraemos dos elementos: 
1. Estadísticas de lluvia (60-90) días, temperatura (60-90) y deforestación (20 años), para tener un proxy de calidad. Si contamos con lluvias altas o bajas, se señalarán unos umbrales (verde-naranja-rojo), así como temperatura, pues ante altas temperaturas, disminuye la cantidad de agua. También se reporta la deforestación histórica, pues ante menor cobertura de bosque, mayor cantidad de sedimentos. Esto informa al usuario sobre posible calidad del agua en la cuenca.
2. Mapas de deforestación histórica. Orientado a tomadores de decisiónes para que identifiquen cuales zonas son las que no tiene bosques, y además han sido transformadas recientemente. La idea es poder atacar la raiz del problema: la deforestación reduce regulación hídrica y calidad del agua, pero a su vez genera gases efecto invernadero y reduce captura de carbono, que causan el cambio climático.


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
