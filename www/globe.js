let globe, renderer, controls;
let globeRotationEnabled = true;
let pendingFocus = null;
const countriesData = new Map();

document.addEventListener('DOMContentLoaded', function () {
  const container = document.getElementById('globe-container');
  const scene = new THREE.Scene();

  const camera = new THREE.PerspectiveCamera(40, container.clientWidth / container.clientHeight, 0.1, 1000);
  camera.position.z = 3.5;

  renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true });
  renderer.setSize(container.clientWidth, container.clientHeight);
  renderer.setPixelRatio(window.devicePixelRatio);
  renderer.setClearColor(0x000000, 0);
  container.appendChild(renderer.domElement);

  scene.add(new THREE.AmbientLight(0xffffff, 0.8));
  const dirLight = new THREE.DirectionalLight(0xffffff, 0.6);
  dirLight.position.set(5, 3, 5);
  scene.add(dirLight);

  const globeGroup = new THREE.Group();
  scene.add(globeGroup);

  const globeGeometry = new THREE.SphereGeometry(1, 64, 64);
  const globeMaterial = new THREE.MeshPhongMaterial({
    color: 0x003366,
    shininess: 10,
    transparent: true,
    opacity: 0.95,
  });
  globe = new THREE.Mesh(globeGeometry, globeMaterial);
  globeGroup.add(globe);

  const countriesGroup = new THREE.Group();
  globeGroup.add(countriesGroup);

  controls = new THREE.OrbitControls(camera, renderer.domElement);
  controls.enableZoom = true;
  controls.enableDamping = true;
  controls.dampingFactor = 0.1;

  // À adapter selon où est ton fichier countries.geojson dans www/
  fetch("countries.geojson")
    .then(res => {
      if (!res.ok) {
        throw new Error(`Erreur chargement countries.geojson: ${res.status}`);
      }
      return res.json();
    })
    .then(data => {
      data.features.forEach(feature => {
        const coords = feature.geometry.coordinates;
        const type = feature.geometry.type;
        const name = feature.properties.name;

        let polygons = [];
        if (type === "Polygon") {
          polygons = [coords];
        } else if (type === "MultiPolygon") {
          polygons = coords;
        }

        let countryPolygons = [];
        let latLonPoints = [];

        polygons.forEach(polygon => {
          polygon.forEach(ring => {
            let prevLon = null;
            ring.forEach(coord => {
              let [lon, lat] = coord;
              if (prevLon !== null) {
                const diff = lon - prevLon;
                if (diff > 180) lon -= 360;
                if (diff < -180) lon += 360;
              }
              prevLon = lon;
              latLonPoints.push([lat, lon]);
            });
          });
        });

        // Dessiner les contours des polygones
        polygons.forEach(polygon => {
          polygon.forEach(ring => {
            const points = [];
            let prevLon = null;
            ring.forEach(coord => {
              let [lon, lat] = coord;
              if (prevLon !== null) {
                const diff = lon - prevLon;
                if (diff > 180) lon -= 360;
                if (diff < -180) lon += 360;
              }
              prevLon = lon;
              const phi = (90 - lat) * (Math.PI / 180);
              const theta = -lon * (Math.PI / 180);
              const x = Math.sin(phi) * Math.cos(theta);
              const y = Math.cos(phi);
              const z = Math.sin(phi) * Math.sin(theta);
              points.push(new THREE.Vector3(x, y, z));
            });
            const geometry = new THREE.BufferGeometry().setFromPoints(points);
            const material = new THREE.LineBasicMaterial({
              color: 0xffffff,
            });
            const line = new THREE.Line(geometry, material);
            line.userData = { name: name };
            countriesGroup.add(line);
            countryPolygons.push(line);
          });
        });

        countriesData.set(name, {
          polygons: countryPolygons,
          latLonPoints: latLonPoints
        });
      });

      console.log("🌐 Globe prêt – données pays chargées");

      if (pendingFocus && countriesData.has(pendingFocus.destination)) {
        console.log("Application différée du focus pour", pendingFocus.destination);
        globe.focusOnCountry(pendingFocus.destination);
        globe.setCountryPolygonColor(pendingFocus.destination, pendingFocus.color || "#66cc66");
        globe.createFilledPolygon(pendingFocus.destination, pendingFocus.color || "#66cc66");
        globeRotationEnabled = false;
        pendingFocus = null;
      }

      if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
        Shiny.setInputValue("globe_ready", true);
      }
    })
    .catch(err => {
      console.error(err);
    });

  globe.createFilledPolygon = function(countryName, colorHex) {
    const data = countriesData.get(countryName);
    if (!data) {
      console.warn("⚠️ Pays", countryName, "non prêt pour remplissage");
      return;
    }
    if (data.filledMesh) {
      globeGroup.remove(data.filledMesh);
      data.filledMesh.geometry.dispose();
      data.filledMesh.material.dispose();
      data.filledMesh = null;
    }
    const shape = new THREE.Shape();
    data.latLonPoints.forEach(([lat, lon], i) => {
      const x = lon;
      const y = lat;
      if (i === 0) shape.moveTo(x, y);
      else shape.lineTo(x, y);
    });
    const geometry = new THREE.ShapeGeometry(shape);
    const pos = geometry.attributes.position;
    for (let i = 0; i < pos.count; i++) {
      const x = pos.getX(i);
      const y = pos.getY(i);
      const phi = (90 - y) * (Math.PI / 180);
      const theta = -x * (Math.PI / 180);
      const newX = Math.sin(phi) * Math.cos(theta);
      const newY = Math.cos(phi);
      const newZ = Math.sin(phi) * Math.sin(theta);
      pos.setXYZ(i, newX, newY, newZ);
    }
    pos.needsUpdate = true;
    geometry.computeVertexNormals();

    const material = new THREE.MeshPhongMaterial({
      color: colorHex || 0x66cc66,
      transparent: true,
      opacity: 0.6,
      side: THREE.DoubleSide,
      depthWrite: false
    });
    const mesh = new THREE.Mesh(geometry, material);
    globeGroup.add(mesh);
    data.filledMesh = mesh;
  };


globe.focusOnCountry = function(countryName) {
  const data = countriesData.get(countryName);
  if (!data) {
    console.warn("⚠️ Pays pour focus non trouvé:", countryName);
    return;
  }

  const [lat, lon] = getCountryCenter(data.latLonPoints);
  const phi = (90 - lat) * (Math.PI / 180);
  const theta = -lon * (Math.PI / 180);

  // Position du pays sur la sphère (vecteur unitaire)
  const countryVector = new THREE.Vector3(
    Math.sin(phi) * Math.cos(theta),
    Math.cos(phi),
    Math.sin(phi) * Math.sin(theta)
  );

  // Vecteur frontal (direction caméra vers centre globe)
  const frontVector = new THREE.Vector3(0, 0, 1);

  // Axe et angle de rotation
  const axis = new THREE.Vector3().crossVectors(countryVector, frontVector).normalize();
  let angle = countryVector.angleTo(frontVector);

  if (axis.length() < 0.00001) {
    axis.set(0, 1, 0); 
    if (angle > Math.PI / 2) angle = Math.PI;
    else angle = 0;
  }

  // Rotation principale (focus)
  const quaternion = new THREE.Quaternion().setFromAxisAngle(axis, angle);
  globeGroup.quaternion.copy(quaternion);

  // Rotation supplémentaire autour de X de -15°
  const rotX = new THREE.Quaternion().setFromAxisAngle(new THREE.Vector3(1, 0, 0), 1 * (Math.PI / 180));
  globeGroup.quaternion.premultiply(rotX);

  // Rotation supplémentaire autour de Y de -10°
  const rotY = new THREE.Quaternion().setFromAxisAngle(new THREE.Vector3(0, 1, 0), 1 * (Math.PI / 180));
  globeGroup.quaternion.premultiply(rotY);

  // Rotation supplémentaire autour de Z de 5°
  const rotZ = new THREE.Quaternion().setFromAxisAngle(new THREE.Vector3(0, 0, 1), -9 * (Math.PI / 180));
  globeGroup.quaternion.premultiply(rotZ);

  // Position caméra fixe devant la Terre
  const cameraDistance = 3.5;
  camera.position.set(0, 0, cameraDistance);
  camera.lookAt(0, 0, 0);
  controls.target.set(0, 0, 0);
  controls.update();

  globeRotationEnabled = false;

  console.log(`🎯 Focus sur ${countryName} puis rotations X -15°, Y -10°, Z 5°`);
};


  globe.setCountryPolygonColor = function(countryName, color) {
    const data = countriesData.get(countryName);
    if (!data) {
      console.warn("⚠️ Polygone de pays", countryName, "non trouvé");
      return;
    }
    data.polygons.forEach(poly => poly.material.color.set(color));
  };

  function getCountryCenter(latLonPoints) {
    let sumLat = 0, sumLon = 0;
    latLonPoints.forEach(([lat, lon]) => {
      sumLat += lat;
      sumLon += lon;
    });
    return [sumLat / latLonPoints.length, sumLon / latLonPoints.length];
  }

  const raycaster = new THREE.Raycaster();
  const mouse = new THREE.Vector2();
  function onMouseClick(event) {
    const rect = renderer.domElement.getBoundingClientRect();
    mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    raycaster.setFromCamera(mouse, camera);
    const intersects = raycaster.intersectObjects(countriesGroup.children);

    if (intersects.length > 0) {
      const country = intersects[0].object.userData.name;
      if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
        Shiny.setInputValue("country_clicked", country, { priority: "event" });
      }
    }
  }
  renderer.domElement.addEventListener("click", onMouseClick);

  window.addEventListener("resize", function () {
    const width = container.clientWidth;
    const height = container.clientHeight;
    camera.aspect = width / height;
    camera.updateProjectionMatrix();
    renderer.setSize(width, height);
  });

  function animate() {
    requestAnimationFrame(animate);

    if (globeRotationEnabled) {
      globeGroup.rotation.y += 0.005;
    }

    if (pendingFocus && countriesData.has(pendingFocus.destination)) {
      console.log("Application différée pendant animate sur", pendingFocus.destination);
      globe.focusOnCountry(pendingFocus.destination);
      globe.setCountryPolygonColor(pendingFocus.destination, pendingFocus.color || "#66cc66");
      globe.createFilledPolygon(pendingFocus.destination, pendingFocus.color || "#66cc66");
      globeRotationEnabled = false;
      pendingFocus = null;
    }

    controls.update();
    renderer.render(scene, camera);
  }

  animate();
});

Shiny.addCustomMessageHandler("stopGlobeRotation", function (message) {
  globeRotationEnabled = false;
  console.log("🛑 Rotation stoppée");
});

Shiny.addCustomMessageHandler("focusGlobeOnCountry", function (message) {
  const countryKey = message.destination;
  if (countriesData.has(countryKey)) {
    globe.focusOnCountry(countryKey);
    globe.setCountryPolygonColor(countryKey, message.color || "#66cc66");
    // Pas besoin de createFilledPolygon si tu ne remplis plus
    globeRotationEnabled = false;
    pendingFocus = null;
  } else {
    console.warn("⚠️ Globe ou pays", countryKey, "non prêt. Mise en attente.");
    pendingFocus = { destination: countryKey, color: message.color };
  }
});

Shiny.addCustomMessageHandler("fillCountryPolygon", function (message) {
  const countryKey = message.destination;
  if (countriesData.has(countryKey)) {
    globe.setCountryPolygonColor(countryKey, message.fillColor || "#66cc66");
    globe.createFilledPolygon(countryKey, message.fillColor || "#66cc66");
  } else {
    console.warn("⚠️ Pays", countryKey, "non prêt pour remplissage.");
    pendingFocus = { destination: countryKey, color: message.fillColor };
  }
});
