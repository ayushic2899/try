package com.subex.ngp.usermanagement.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.ws.rs.NotFoundException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.keycloak.admin.client.resource.ClientResource;
import org.keycloak.admin.client.resource.ClientsResource;
import org.keycloak.admin.client.resource.RolesResource;
import org.keycloak.representations.idm.ClientRepresentation;
import org.keycloak.representations.idm.RoleRepresentation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.subex.ngp.commonservices.model.ObjectAccessModel;
import com.subex.ngp.usermanagement.keycloak.KeyCloakBuilder;
import com.subex.ngp.usermanagement.keycloak.configuration.KeycloakCustomConfig;
import com.subex.ngp.usermanagement.keycloak.helper.DefaultClientConstants;
import com.subex.ngp.usermanagement.model.TrailModel;
import com.subex.ngp.usermanagement.model.ModuleModel;
import com.subex.ngp.usermanagement.model.PrivilegeModel;

@Service
public class TrailServiceImpl {

	private static Logger log = LogManager.getLogger(UserServiceImpl.class);

	@Autowired
	private KeyCloakBuilder keycloakBuilder;

	@Autowired
	private KeycloakCustomConfig keycloakCustomConfig;

	public List<TrailModel> getTrail() {
		List<TrailModel> trailModelList = new ArrayList<TrailModel>();
		ClientsResource clientsResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm())
				.clients();
		List<ClientRepresentation> clientsRepresentation = clientsResource.findAll();
		clientsRepresentation = removeDefaultClients(clientsRepresentation);
		for (ClientRepresentation client : clientsRepresentation) {
			if (client.isPublicClient()) {
				TrailModel trailModel = new TrailModel();
				//trailModel.setId(client.getId());
				trailModel.setClientId(client.getClientId());
				trailModel.setName(client.getName());
				//trailModel.setDescription(client.getDescription());
				//trailModel.setModules(getModules(clientsResource.get(client.getId()), new ArrayList<String>()));
				trailModelList.add(trailModel);
			}
		}

		return trailModelList;
	}
	
	public List<ClientRepresentation> getAllClients() {
		ClientsResource clientsResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm())
				.clients();
		List<ClientRepresentation> clientsRepresentation = clientsResource.findAll();
		clientsRepresentation = removeDefaultClients(clientsRepresentation);
		return clientsRepresentation;
	}
	
	 

	public List<ClientRepresentation> removeDefaultClients(List<ClientRepresentation> clients) {

		List<String> deafultClients = DefaultClientConstants.DEFAULT_CLIENT_LIST;
		ListIterator<ClientRepresentation> listIterator = clients.listIterator();
		while (listIterator.hasNext()) {
			ClientRepresentation client = listIterator.next();
			if (deafultClients.contains(client.getName()))
				listIterator.remove();
		}
		return clients;
	}

	 

	 
}
