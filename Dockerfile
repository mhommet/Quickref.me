# Étape 1 : Build du site Astro
FROM node:20 AS builder
WORKDIR /app
COPY . .
RUN npm install && npm run build

# Étape 2 : Serveur nginx pour le site statique
FROM nginx:alpine
COPY default.conf /etc/nginx/conf.d/default.conf
COPY --from=builder /app/public /usr/share/nginx/html
EXPOSE 80

