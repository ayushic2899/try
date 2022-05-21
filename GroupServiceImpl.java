package com.subex.ngp.usermanagement.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javax.transaction.Transactional;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.keycloak.admin.client.CreatedResponseUtil;
import org.keycloak.admin.client.resource.ClientResource;
import org.keycloak.admin.client.resource.ClientsResource;
import org.keycloak.admin.client.resource.GroupResource;
import org.keycloak.admin.client.resource.GroupsResource;
import org.keycloak.admin.client.resource.RoleMappingResource;
import org.keycloak.admin.client.resource.RoleResource;
import org.keycloak.admin.client.resource.RolesResource;
import org.keycloak.admin.client.resource.UserResource;
import org.keycloak.representations.idm.ClientRepresentation;
import org.keycloak.representations.idm.GroupRepresentation;
import org.keycloak.representations.idm.RoleRepresentation;
import org.keycloak.representations.idm.UserRepresentation;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.subex.ngp.audit.trail.lib.AuditEventModel;
import com.subex.ngp.usermanagement.entity.ClientEntity;
import com.subex.ngp.usermanagement.entity.GlobalPrivilegeEntity;
import com.subex.ngp.usermanagement.entity.MappingEntity;
import com.subex.ngp.usermanagement.enums.GroupGridComparartor;
import com.subex.ngp.usermanagement.enums.GroupRepresentationComparator;
import com.subex.ngp.usermanagement.helper.UserManagementHelper;
import com.subex.ngp.usermanagement.keycloak.KeyCloakBuilder;
import com.subex.ngp.usermanagement.keycloak.NgpSession;
import com.subex.ngp.usermanagement.keycloak.configuration.KeycloakCustomConfig;
import com.subex.ngp.usermanagement.keycloak.helper.GroupAttribute;
import com.subex.ngp.usermanagement.model.ApplicationModel;
import com.subex.ngp.usermanagement.model.GroupDetailModel;
import com.subex.ngp.usermanagement.model.GroupGridModel;
import com.subex.ngp.usermanagement.model.GroupModel;
import com.subex.ngp.usermanagement.model.GroupUsersModel;
import com.subex.ngp.usermanagement.model.IdModel;
import com.subex.ngp.usermanagement.model.ModuleModel;
import com.subex.ngp.usermanagement.model.PrivilegeModel;
import com.subex.ngp.usermanagement.model.SearchResultItemModel;
import com.subex.ngp.usermanagement.repository.ClientRepository;
import com.subex.ngp.usermanagement.repository.GlobalPrivilegeRepository;
import com.subex.ngp.usermanagement.repository.MappingRepository;
import com.subex.ngp.usermanagement.repository.ParentClientRepository;

@Component
public class GroupServiceImpl {

	@Autowired
	private KeyCloakBuilder keyCloakBuilder;

	@Autowired
	private KeycloakCustomConfig keyCloakCustomConfig;

	@Autowired
	private ApplicationServiceImpl aplicationServiceImpl;

	@Autowired
	private NgpSession session;

	@Autowired
	private MappingRepository mappingRepo;

	@Autowired
	private GlobalPrivilegeRepository globalRepo;
	
	@Autowired
	private ParentClientRepository parentClientRepo;
	
	@Autowired
	private ClientRepository clientRepo;
	
	@SuppressWarnings("unused")
	private static Logger log = LogManager.getLogger(GroupServiceImpl.class);

	private static final int DEFAULT_PAGE_INDEX = 0;

	private static final String USER_ID = "userId";

	private static final String GROUP_FILTER_PREFIX = "name:";

	public boolean validate(GroupModel groupModel) {
		if (groupModel == null)
			return false;
		if (StringUtils.isEmpty(groupModel.getName()))
			return false;
		return true;

	}

	public boolean existsByGroupName(String groupName) {

		List<GroupRepresentation> keycloakGroupRepresentations = keyCloakBuilder.getInstance()
				.realm(keyCloakCustomConfig.getRealm()).groups().groups();

		for (GroupRepresentation groupRepresentation : keycloakGroupRepresentations) {
			if (groupRepresentation.getName().equalsIgnoreCase(groupName))
				return true;
		}
		return false;
	}

	public IdModel createGroup(GroupModel groupModel) {

		List<List<ApplicationModel>> userGroupModelList = new ArrayList();
		List<String> groupIds = new ArrayList();
		List<String> tags = groupModel.getTags();
		if(tags == null) {
			tags = Collections.emptyList();
		}
		if(tags.size() >0) {
		for (int i=0;i<tags.size();i++) {
			String id = getKeyCloakIdFromName(tags.get(i));
			userGroupModelList.add(getGroupApplications(id));
			groupIds.add(id);
		}
		}

		if(userGroupModelList.size() >0) {
		List<ApplicationModel> applicationModelList = mergeApplicationModules(userGroupModelList);
		groupModel.applications(new ArrayList());
    	for (int i=0;i<applicationModelList.size();i++) {
    		groupModel.addApplicationsItem(applicationModelList.get(i));
    	}
		}

		GroupRepresentation groupRepresentations = convertGroupModelToGroupRepresentation(groupModel);
		Response response = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
				.add(groupRepresentations);

		List<MappingEntity> lstDatasetPrvlges = mappingRepo.getDistinctEntitiesBasedonIds(groupIds);
		List<GlobalPrivilegeEntity> lstGlobalPrvlges = globalRepo.getDistinctGlobalPrivilegesBasedonIds(groupIds);
		if (response.getStatus() == Response.Status.CREATED.getStatusCode()) {
			String uuid = CreatedResponseUtil.getCreatedId(response);
			if(userGroupModelList.size() >0) {
			for(int k=0;k<lstDatasetPrvlges.size();k++) {
				MappingEntity mapEntity = lstDatasetPrvlges.get(k);
				MappingEntity newntity = new MappingEntity();

				newntity.setDynamicEntity(mapEntity.getDynamicEntity());
				newntity.setGroupId(uuid);
				newntity.setPrivilegeEntity(mapEntity.getPrivilegeEntity());
				mappingRepo.save(newntity);
			}
			
			for(int k=0;k<lstGlobalPrvlges.size();k++) {
				GlobalPrivilegeEntity globalPrvlg = lstGlobalPrvlges.get(k);
				GlobalPrivilegeEntity newPrvlg = new GlobalPrivilegeEntity();

				newPrvlg.setGroupId(uuid);
				newPrvlg.setPrivilegeEntity(globalPrvlg.getPrivilegeEntity());
				newPrvlg.setIsEnabled(globalPrvlg.getIsEnabled());
				globalRepo.save(newPrvlg);
			}
			}
			addClientRoles(uuid, groupModel);
			return new IdModel().id(uuid);
		} else {
			return null;
		}
	}



	public List<ApplicationModel> mergeApplicationModules(List<List<ApplicationModel>> userGroupModelList){
		List<ApplicationModel> applicationModelList = new ArrayList<>();
		for (int i=0;i<userGroupModelList.size();i++) {

		    for (int j = 0; j < userGroupModelList.get(i).size(); j++)
		    {
		    	ApplicationModel applicationModel = userGroupModelList.get(i).get(j);
		    	List<List<ModuleModel>> moduleLst = new ArrayList<>();
		    	for (int k=0;k<userGroupModelList.size();k++) {
		    		ApplicationModel am = userGroupModelList.get(k).get(j);
		    		moduleLst.add(am.getModules());
		    	}
		    	List<ModuleModel> mergedModules = mergeModule(moduleLst);
		    	applicationModel.modules(null);
	    		if(mergedModules.size() == 0) {
	    			List<ModuleModel> emptylist = Collections.emptyList();
		    		applicationModel.modules(emptylist) ;
	    		}
		    	for (int k=0;k<mergedModules.size();k++) {
		    		applicationModel.addModulesItem(mergedModules.get(k));
		    	}
		    	applicationModelList.add(applicationModel);
		    }
			break;
		}
		return applicationModelList;
	}
	public List<ModuleModel> mergeModule(List<List<ModuleModel>> moduleLst){
		ModuleModel model = new ModuleModel();

		List<ModuleModel> mergedModule = new ArrayList<>();
		for (int i=0;i<moduleLst.size();i++) {

		    for (int j = 0; j < moduleLst.get(i).size(); j++)
		    {
		    	List<List<PrivilegeModel>> prvlges = new ArrayList<>();
		    	ModuleModel moduleModel = new ModuleModel();
		    	for (int k=0;k<moduleLst.size();k++) {

		    		ModuleModel am = moduleLst.get(k).get(j);
		    		moduleModel = am;
		    		if(am.getIsSelected() == true) {
		    			moduleModel = am;
		    		}
		    		prvlges.add(am.getPrivileges());

		    		if(am.getIsSelected() == true) {
		    			break;
		    		}
		    	}
		    	List<PrivilegeModel> mergedPrvlges = mergePrivileges(prvlges);
		    	moduleModel.privileges(new ArrayList());
		    	for (int k=0;k<mergedPrvlges.size();k++) {
		    		moduleModel.addPrivilegesItem(mergedPrvlges.get(k));
		    	}
		    	mergedModule.add(moduleModel);
		    }
			break;
		}

		return mergedModule;
	}

	public List<PrivilegeModel> mergePrivileges(List<List<PrivilegeModel>> prvlgLst){
		PrivilegeModel model = new PrivilegeModel();
		List<PrivilegeModel> prvlges = new ArrayList<>();
		for (int i=0;i<prvlgLst.size();i++) {

		    for (int j = 0; j < prvlgLst.get(i).size(); j++){
		    	PrivilegeModel mergedPrvlegModel = new PrivilegeModel();
		    	for (int k=0;k<prvlgLst.size();k++) {
		    		
		    		PrivilegeModel prvlegModel = prvlgLst.get(k).get(j);
		    		mergedPrvlegModel = prvlegModel;
		    		if(prvlegModel.getIsSelected() == true) {
		    			mergedPrvlegModel = prvlegModel;
		    			break;
		    		}
		    }
		    	prvlges.add(mergedPrvlegModel);
		    }
			break;
		}
		return prvlges;
	}



	private void addClientRoles(String uuid, GroupModel groupModel) {

		try {
			GroupResource groupResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
					.group(uuid);
			ClientsResource clients = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).clients();
			List<ApplicationModel> applicationModels = groupModel.getApplications();
			for (ApplicationModel applicationModel : applicationModels) {
				List<RoleRepresentation> rolesList = new ArrayList<RoleRepresentation>();

				String clientId = applicationModel.getId();
				ClientResource clientResource = clients.get(clientId);
				List<RoleRepresentation> deleteRolesList = new ArrayList<RoleRepresentation>();

				deleteRolesList = groupResource.roles().clientLevel(clientId).listAll();

				RolesResource rolesRessource = clientResource.roles();
				List<ModuleModel> modules = applicationModel.getModules();
				for (ModuleModel module : modules) {

					List<PrivilegeModel> privilege = module.getPrivileges();
					for (PrivilegeModel privilegeModel : privilege) {

						addRoles(privilegeModel, rolesList, rolesRessource, deleteRolesList);
					}
					if (module.getIsSelected() && privilege.isEmpty()) {
						RoleResource roleResource = rolesRessource.get(module.getName());
						rolesList.add(roleResource.toRepresentation());
					}

				}

				groupResource.roles().clientLevel(clientId).add(rolesList);
				if (!deleteRolesList.isEmpty())
					groupResource.roles().clientLevel(clientId).remove(deleteRolesList);

			}
		} catch (Exception e) {

		}
	}

	private void addRoles(PrivilegeModel privilegeModel, List<RoleRepresentation> rolesList,
			RolesResource rolesRessource, List<RoleRepresentation> deleteRolesList) {

		List<PrivilegeModel> children = privilegeModel.getChildren();
		if (children.size() > 0 && children != null) {
			for (PrivilegeModel childPrivilegeModel : children) {
				addRoles(childPrivilegeModel, rolesList, rolesRessource, deleteRolesList);
			}
		} else {
			if (privilegeModel.getIsSelected()) {
				RoleResource roleResource = rolesRessource.get(privilegeModel.getName());
				rolesList.add(roleResource.toRepresentation());
				deleteRolesList.remove(roleResource.toRepresentation());
			}
		}

	}

	private void updateClientRoles(String uuid, GroupModel groupModel) {

		try {
			GroupResource groupResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
					.group(uuid);
			ClientsResource clients = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).clients();
			List<ApplicationModel> applicationModels = groupModel.getApplications();
			for (ApplicationModel applicationModel : applicationModels) {
				List<RoleRepresentation> rolesList = new ArrayList<RoleRepresentation>();

				String clientId = applicationModel.getId();
				ClientResource clientResource = clients.get(clientId);
				List<RoleRepresentation> deleteRolesList = new ArrayList<RoleRepresentation>();

				deleteRolesList = groupResource.roles().clientLevel(clientId).listEffective();

				RolesResource rolesRessource = clientResource.roles();
				List<ModuleModel> modules = applicationModel.getModules();
				for (ModuleModel module : modules) {

					List<PrivilegeModel> privilege = module.getPrivileges();
					for (PrivilegeModel privilegeModel : privilege) {

						updateRoles(privilegeModel, rolesList, rolesRessource, deleteRolesList);
					}

					if (module.getIsSelected() && privilege.isEmpty()) {
						RoleResource roleResource = rolesRessource.get(module.getName());
						rolesList.add(roleResource.toRepresentation());
						deleteRolesList
								.removeIf(x -> x.getName().equalsIgnoreCase(roleResource.toRepresentation().getName()));
					}

				}

				groupResource.roles().clientLevel(clientId).add(rolesList);
				if (!deleteRolesList.isEmpty())
					groupResource.roles().clientLevel(clientId).remove(deleteRolesList);

			}
		} catch (Exception e) {

		}
	}

	private void updateRoles(PrivilegeModel privilegeModel, List<RoleRepresentation> rolesList,
			RolesResource rolesRessource, List<RoleRepresentation> deleteRolesList) {

		List<PrivilegeModel> children = privilegeModel.getChildren();
		if (children != null && children.size() > 0) {
			for (PrivilegeModel childPrivilegeModel : children) {
				updateRoles(childPrivilegeModel, rolesList, rolesRessource, deleteRolesList);
			}
		} else {
			if (privilegeModel.getIsSelected()) {
				RoleResource roleResource = rolesRessource.get(privilegeModel.getName());
				rolesList.add(roleResource.toRepresentation());
				deleteRolesList.removeIf(x -> x.getName().equalsIgnoreCase(roleResource.toRepresentation().getName()));
			}
		}

	}

	public GroupRepresentation convertGroupModelToGroupRepresentation(GroupModel groupModel) {

		GroupRepresentation groupRepresentation = new GroupRepresentation();
		Date date = new Date();

		groupRepresentation.setName(groupModel.getName());

		if (groupModel.getNotes() != null)
			groupRepresentation.singleAttribute(GroupAttribute.NOTES.getLabel(), groupModel.getNotes());

		if (groupModel.getTags() != null)
		groupRepresentation.singleAttribute(GroupAttribute.TAG.getLabel(), String.valueOf(groupModel.getTags().stream()
				.filter(tag -> !StringUtils.isEmpty(tag)).collect(Collectors.toList())));

		groupRepresentation.singleAttribute(GroupAttribute.CREATED_BY.getLabel(), session.getUserId());

		groupRepresentation.singleAttribute(GroupAttribute.CREATED_AT.getLabel(), String.valueOf(date.getTime()));

		groupRepresentation.singleAttribute(GroupAttribute.MODIFIED_AT.getLabel(), String.valueOf(date.getTime()));

		groupRepresentation.singleAttribute(GroupAttribute.DELETE_FLAG.getLabel(), Boolean.FALSE.toString());

		groupRepresentation.singleAttribute(GroupAttribute.MODIFIED_BY.getLabel(), session.getUserId());

		return groupRepresentation;

	}

	public boolean existsById(String id) {

		if (StringUtils.isEmpty(id))
			return false;

		GroupRepresentation groupRepresentation = findById(id);

		if (groupRepresentation == null)
			return false;

		return true;
	}

	public GroupRepresentation findById(String id) {
		try {
			GroupRepresentation groupRepresentation = keyCloakBuilder.getInstance()
					.realm(keyCloakCustomConfig.getRealm()).groups().group(id).toRepresentation();
			return groupRepresentation;
		} catch (NotFoundException nfe) {
			return null;
		}
	}

	private GroupResource fetchGroupResource(String groupUUID) {

		GroupResource groupResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
				.group(groupUUID);
		return groupResource;
	}

	public void updateAttribute(String id, Map<String, List<String>> attributesMap) {

		if (attributesMap == null)
			return;

		GroupResource groupResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
				.group(id);

		GroupRepresentation groupRepresentation = groupResource.toRepresentation();

		if (groupRepresentation.getAttributes() == null)
			groupRepresentation.setAttributes(new HashMap<String, List<String>>());

		attributesMap.forEach((key, value) -> {
			groupRepresentation.getAttributes().put(key, value);
		});

		Date date = new Date();

		groupRepresentation.singleAttribute(GroupAttribute.MODIFIED_BY.getLabel(), session.getUserId());
		groupRepresentation.singleAttribute(GroupAttribute.MODIFIED_AT.getLabel(), String.valueOf(date.getTime()));

		groupResource.update(groupRepresentation);

	}

	@Transactional
	public void delete(String groupUUID) {
		GroupResource groupResource = fetchGroupResource(groupUUID);
		groupResource.remove();
		try {
			mappingRepo.deleteByGroupId(groupUUID);
		} catch(Exception e) {
			e.printStackTrace();
		}
	}

	public boolean isDeleted(String id) {

		GroupRepresentation groupRepresentation = findById(id);

		if (groupRepresentation != null) {
			List<String> values = getValue(groupRepresentation, GroupAttribute.DELETE_FLAG);

			if (values.size() == 1 && values.get(0).equalsIgnoreCase(Boolean.TRUE.toString()))
				return true;
		}
		return false;
	}

	private List<String> getValue(GroupRepresentation groupRepresentation, GroupAttribute groupAttribute) {

		List<String> values = groupRepresentation.getAttributes().get(groupAttribute.getLabel());

		if (values == null)
			return new ArrayList<String>();

		return groupRepresentation.getAttributes().get(groupAttribute.getLabel());
	}

	public void softDeleteGroup(String id) {

		Map<String, List<String>> attributesMap = new HashMap<String, List<String>>();
		attributesMap.put(GroupAttribute.DELETE_FLAG.getLabel(),
				new ArrayList<String>(Arrays.asList(Boolean.TRUE.toString())));

		updateAttribute(id, attributesMap);

	}

	public List<ApplicationModel> getGroupApplications(String id) {

		List<ApplicationModel> applicationModelList = new ArrayList<ApplicationModel>();
		GroupResource groupResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
				.group(id);
		RoleMappingResource groupRoles = groupResource.roles();
		ClientsResource clientsResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm())
				.clients();
		List<ClientRepresentation> clientsRepresentation = clientsResource.findAll();
		clientsRepresentation = aplicationServiceImpl.removeDefaultClients(clientsRepresentation);
		for (ClientRepresentation client : clientsRepresentation) {
			if (client.isPublicClient()) {
				ApplicationModel applicationModel = new ApplicationModel();
				applicationModel.setId(client.getId());
				applicationModel.setClientId(client.getClientId());
				applicationModel.setName(client.getName());
				applicationModel.setDescription(client.getDescription());
				List<RoleRepresentation> roleRepresentationList = groupRoles.clientLevel(client.getId())
						.listEffective();
				applicationModel.setModules(aplicationServiceImpl.getModules(clientsResource.get(client.getId()),
						getGroupRoles(roleRepresentationList)));
				applicationModelList.add(applicationModel);
			}

		}

		return applicationModelList;
	}

	@Transactional
	public List<ApplicationModel> getGroupApplicationForParent(String id, String parentName) throws Exception {
		List<ApplicationModel> applicationModelList = new ArrayList<ApplicationModel>();

		List<String> clientIds = parentClientRepo.getClientNameBasedOnParentName(parentName);
		if(clientIds.size() == 0) {
			Optional<ClientEntity> client = clientRepo.findByClientId(parentName);
			if(client.isPresent()) {
				clientIds.add(client.get().getClientId());	
			}
		}
		for(String client:clientIds) {
			ApplicationModel appModel = new ApplicationModel();
			appModel = getGroupApplicationForClient(id,client);
			applicationModelList.add(appModel);
		}
		
		return applicationModelList;
	}

	public ApplicationModel getGroupApplicationForClient(String id, String clientName) throws Exception {
		List<ApplicationModel> applicationModelList = new ArrayList<ApplicationModel>();
		GroupResource groupResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
				.group(id);
		RoleMappingResource groupRoles = groupResource.roles();
		ClientsResource clientsResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm())
				.clients();
		List<ClientRepresentation> clientsRepresentation = clientsResource.findAll();
		clientsRepresentation = aplicationServiceImpl.removeDefaultClients(clientsRepresentation);
		for (ClientRepresentation client : clientsRepresentation) {
			if (client.isPublicClient() && clientName.equalsIgnoreCase(client.getClientId())) {
				ApplicationModel applicationModel = new ApplicationModel();
				applicationModel.setId(client.getId());
				applicationModel.setClientId(client.getClientId());
				applicationModel.setName(client.getName());
				applicationModel.setDescription(client.getDescription());
				List<RoleRepresentation> roleRepresentationList = groupRoles.clientLevel(client.getId()).listEffective();
				applicationModel.setModules(aplicationServiceImpl.getModules(clientsResource.get(client.getId()),
						getGroupRoles(roleRepresentationList)));
				applicationModelList.add(applicationModel);
			}
		}
		if(applicationModelList.size()==0) {
			throw new Exception("Client not found");
		}
		return applicationModelList.get(0);
	}

	private List<String> getGroupRoles(List<RoleRepresentation> roleRepresentationList) {

		return roleRepresentationList.stream().map(x -> x.getName()).collect(Collectors.toList());

	}

	public Optional<GroupDetailModel> getGroupDetails(String id) {
		GroupResource groupRosource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
				.group(id);
		GroupRepresentation groupRepresentation = null;

		try {
			groupRepresentation = groupRosource.toRepresentation();
		} catch (javax.ws.rs.NotFoundException e) {
			return Optional.empty();
		}

		int noOfActiveUsers = getActiveUsersCountIngroup(groupRosource);
		GroupDetailModel groupModel = convertGroupRepresentationToGroupDetailModel(groupRepresentation);
		groupModel.setApplications(getGroupApplications(id));
		groupModel.setNoOfActiveUsers(noOfActiveUsers);

		AuditEventModel.callAuditLog("GROUP", "VIEW",
				"Details of  Group " + groupModel.getGroupName() +" Viewed  by "+MDC.get(USER_ID),
				"Details of  Group viewed Succesfully");
		log.debug("Group Details Viewed Successfully by :{}",MDC.get(USER_ID));
		return Optional.of(groupModel);
	}

	private int getActiveUsersCountIngroup(GroupResource groupRosource) {
		List<UserRepresentation> members = groupRosource.members(0, Integer.MAX_VALUE);
//		return (int) members.stream().filter(x -> x.isEnabled() && !isDeleted(x)).count();
		return (int) members.stream().filter(x -> x.isEnabled()).count();
	}

	public boolean isDeleted(UserRepresentation userRepresentation) {
		List<String> list = userRepresentation.getAttributes().get(GroupAttribute.DELETE_FLAG.getLabel());
		if (list.size() > 0)
			return Boolean.valueOf(list.get(0));

		return true;
	}

	public GroupDetailModel convertGroupRepresentationToGroupDetailModel(GroupRepresentation groupRepresentation) {

		GroupDetailModel groupModel = new GroupDetailModel();
		Map<String, List<String>> attributes = groupRepresentation.getAttributes();
		groupModel.setGroupName(groupRepresentation.getName());
		groupModel.setCreatedBy((String) getAttributeValue(attributes, GroupAttribute.CREATED_BY.getLabel()));
		groupModel.setCreatedOn((String) getAttributeValue(attributes, GroupAttribute.CREATED_AT.getLabel()));
		groupModel.setDescription((String) getAttributeValue(attributes, GroupAttribute.NOTES.getLabel()));
		return groupModel;

	}

	private Object getAttributeValue(Map<String, List<String>> attributes, String key) {
		if (!CollectionUtils.isEmpty(attributes.get(key)))
			return attributes.get(key).get(0);

		return null;
	}

	public boolean existsByIdWithName(String id, String groupName) {

		GroupRepresentation groupRepresentation = findByGroupName(groupName);

		if (groupRepresentation == null || groupRepresentation.getId().equalsIgnoreCase(id))
			return true;
		return false;
	}

	public GroupRepresentation findByGroupName(String name) {

		try {

			List<GroupRepresentation> keycloakGroupRepresentations = keyCloakBuilder.getInstance()
					.realm(keyCloakCustomConfig.getRealm()).groups().groups();
			for (GroupRepresentation groupRepresentation : keycloakGroupRepresentations) {
				if (groupRepresentation.getName().equalsIgnoreCase(name))
					return groupRepresentation;
			}
			return null;
		} catch (NotFoundException nfe) {
			return null;
		}
	}

	@Transactional
	public void updateGroup(String id, GroupModel groupModel) {

		List<List<ApplicationModel>> userGroupModelList = new ArrayList();
		List<String> groupIds = new ArrayList();
		List<String> tags = groupModel.getTags();

		if(tags.size() >0) {
		for (int i=0;i<tags.size();i++) {
			String groupId = getKeyCloakIdFromName(tags.get(i));
			userGroupModelList.add(getGroupApplications(groupId));
			groupIds.add(groupId);
		}
		}

		if(userGroupModelList.size() >0) {
		List<ApplicationModel> applicationModelList = mergeApplicationModules(userGroupModelList);

		groupModel.applications(new ArrayList());
    	for (int i=0;i<applicationModelList.size();i++) {
    		groupModel.addApplicationsItem(applicationModelList.get(i));
    	}
		}

		GroupResource groupResource = fetchGroupResource(id);

		GroupRepresentation groupRepresentation = groupResource.toRepresentation();

		groupRepresentation.setName(groupModel.getName());
		if (groupModel.getNotes() != null)
			groupRepresentation.singleAttribute(GroupAttribute.NOTES.getLabel(), groupModel.getNotes());
		if (groupModel.getTags() != null)
			groupRepresentation.getAttributes().put(GroupAttribute.TAG.getLabel(), groupModel.getTags().stream()
					.filter(tag -> !StringUtils.isEmpty(tag)).collect(Collectors.toList()));
		groupRepresentation.singleAttribute(GroupAttribute.MODIFIED_BY.getLabel(), session.getUserId());
		groupRepresentation.singleAttribute(GroupAttribute.MODIFIED_AT.getLabel(),
				String.valueOf(System.currentTimeMillis()));

		updateClientRoles(id, groupModel);

		groupResource.update(groupRepresentation);
		GroupRepresentation groupRepresentations = convertGroupModelToGroupRepresentation(groupModel);
		Response response = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
				.add(groupRepresentations);

		List<MappingEntity> lstDatasetPrvlges = mappingRepo.getDistinctEntitiesBasedonIds(groupIds);
		List<GlobalPrivilegeEntity> lstGlobalPrvlges = globalRepo.getDistinctGlobalPrivilegesBasedonIds(groupIds);
		if(userGroupModelList.size() >0) {
			mappingRepo.deleteByGroupId(id);
			globalRepo.deleteByGroupId(id);
		for(int k=0;k<lstDatasetPrvlges.size();k++) {
			MappingEntity mapEntity = lstDatasetPrvlges.get(k);
			MappingEntity newntity = new MappingEntity();

			newntity.setDynamicEntity(mapEntity.getDynamicEntity());
			newntity.setGroupId(id);
			newntity.setPrivilegeEntity(mapEntity.getPrivilegeEntity());
			mappingRepo.save(newntity);
		}
		for(int k=0;k<lstGlobalPrvlges.size();k++) {
			GlobalPrivilegeEntity globalPrvlg = lstGlobalPrvlges.get(k);
			GlobalPrivilegeEntity newPrvlg = new GlobalPrivilegeEntity();

			newPrvlg.setGroupId(id);
			newPrvlg.setPrivilegeEntity(globalPrvlg.getPrivilegeEntity());
			newPrvlg.setIsEnabled(globalPrvlg.getIsEnabled());
			globalRepo.save(newPrvlg);
		}
		}
	}

	public long getCountOfGroups(String filter) {

		List<GroupRepresentation> groupRepresentations = keyCloakBuilder.getInstance()
				.realm(keyCloakCustomConfig.getRealm()).groups().groups();

		return groupRepresentations.stream().filter(getGroupListFilter(filter, true)).count();
	}

	private Predicate<GroupRepresentation> getGroupListFilter(String filter, boolean isAndFilter) {

		Map<String, String> filterMap = UserManagementHelper.convertToMap(filter);

		List<Predicate<GroupRepresentation>> predicates = new ArrayList<Predicate<GroupRepresentation>>();

		if (filterMap.containsKey("name"))
		{
			log.debug("{} has  performed on search on {} group in group detail",MDC.get(USER_ID),filterMap.get("name"));

			predicates.add(getUserSearchQueryFilter(filterMap.get("name")));
		}

		if (isAndFilter)
			return predicates.stream().reduce(Predicate::and).orElse(x -> true);
		else
			return predicates.stream().reduce(Predicate::or).orElse(x -> true);
	}

	private Predicate<GroupRepresentation> getUserSearchQueryFilter(String str) {

		List<Predicate<GroupRepresentation>> predicates = new ArrayList<Predicate<GroupRepresentation>>();

		if (StringUtils.isEmpty(str))
			return predicates.stream().reduce(Predicate::or).orElse(groupRepresentation -> true);

		final String value = str.toLowerCase();

		predicates.add(groupRepresentation -> groupRepresentation.getName() != null
				&& groupRepresentation.getName().toLowerCase().contains(value));

		/*
		 * predicates.add(groupRepresentation -> { GroupResource group =
		 *
		 * keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
		 * .group(groupRepresentation.getId()); Map<String, List<String>> attributes =
		 * group.toRepresentation().getAttributes(); return attributes != null &&
		 * !CollectionUtils.isEmpty(attributes.get(GroupAttribute.NOTES.getLabel())) &&
		 * attributes.get(GroupAttribute.NOTES.getLabel()).get(0).toLowerCase().contains
		 * (value); });
		 */

		return predicates.stream().reduce(Predicate::or).orElse(groupRepresentation -> true);
	}

	public List<GroupGridModel> getGroupsListing(Integer pageIndex, Integer pageSize, String filter, String sort,
			boolean isAndFilter) {

		GroupsResource groupsResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups();

		/*
		 * List<GroupRepresentation> groupRepresentations =
		 * keyCloakBuilder.getInstance()
		 * .realm(keyCloakCustomConfig.getRealm()).groups().groups();
		 */

		List<GroupRepresentation> groupRepresentations = groupsResource.groups();

		List<GroupGridModel> groupListModels = new ArrayList<>();

		List<GroupRepresentation> groupRepresentationFiltered = getPage(
				groupRepresentations.stream().sorted(getGroupListRepresntSort(sort))
						.filter(getGroupListFilter(filter, isAndFilter)).collect(Collectors.toList()),
				pageIndex, pageSize);

		// List<String> clientIds = getClientIdList();

//		groupRepresentations.stream().filter(getGroupListFilter(filter, isAndFilter)).filter(getDeleteFilter())
		groupRepresentationFiltered.stream().forEach(groupRepresentation -> {

			groupListModels.add(convertGroupRepresentationToGroupGridModel(groupRepresentation, groupsResource,
					getUsersCount(groupRepresentation.getId()), null, true));
		});

		return groupListModels;

	}

	public List<SearchResultItemModel> getGroupsListingGlobal(Integer pageIndex, Integer pageSize, String filter,
			String sort, boolean isAndFilter) {

		GroupsResource groupsResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups();
		List<GroupRepresentation> groupRepresentations1 = groupsResource.groups();
//		List<GroupRepresentation> groupRepresentations = groupsResource.groups(filter, 0, Integer.MAX_VALUE);
		if (filter != null) {
			filter = GROUP_FILTER_PREFIX + filter;
		}
		List<GroupRepresentation> groupRepresentations = getPage(
				groupRepresentations1.stream().sorted(getGroupListRepresntSort(sort))
						.filter(getGroupListFilter(filter, isAndFilter)).collect(Collectors.toList()),
				pageIndex, pageSize);

		List<SearchResultItemModel> modelList = new ArrayList<>();

		for (GroupRepresentation groupRepresentation : groupRepresentations) {
			SearchResultItemModel model = new SearchResultItemModel();
			model.setValue(groupRepresentation.getName());
			model.setName(groupRepresentation.getName());

			GroupResource group = groupsResource.group(groupRepresentation.getId());

			Map<String, List<String>> attributes = group.toRepresentation().getAttributes();

			if (attributes != null && !CollectionUtils.isEmpty(attributes.get(GroupAttribute.NOTES.getLabel())))
				model.setDescription(attributes.get(GroupAttribute.NOTES.getLabel()).get(0));

			model.setIsLdapUser(null);
			modelList.add(model);
		}

		return modelList;
	}



	/*
	 * public Predicate<GroupRepresentation> getDeleteFilter() {
	 *
	 * return groupRepresentation -> { GroupResource group =
	 * keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).groups()
	 * .group(groupRepresentation.getId()); Map<String, List<String>> attributes =
	 * group.toRepresentation().getAttributes(); return attributes != null &&
	 * !CollectionUtils.isEmpty(attributes.get(GroupAttribute.DELETE_FLAG.getLabel()
	 * )) && attributes.get(GroupAttribute.DELETE_FLAG.getLabel()).get(0)
	 * .equalsIgnoreCase(Boolean.FALSE.toString()); }; }
	 */

	/*
	 * public int getApplicationsCount(String groupUuid) {
	 *
	 * List<ApplicationModel> groupApplications = getGroupApplications(groupUuid);
	 * return getApplicationsCount(groupApplications);
	 *
	 * }
	 *
	 * private int getApplicationsCount(List<ApplicationModel> groupApplications) {
	 * int count = 0; for (ApplicationModel groupApplication : groupApplications) {
	 * List<ModuleModel> modules = groupApplication.getModules(); for (ModuleModel
	 * module : modules) { if (module.getIsSelected()) { count = count + 1; break; }
	 * else { List<PrivilegeModel> privileges = module.getPrivileges(); for
	 * (PrivilegeModel privilege : privileges) { if (getPrevilageCount(privilege) ==
	 * 1) { count = count + 1; break; } } } } }
	 *
	 * return count;
	 *
	 * }
	 *
	 *
	 *
	 * private int getPrevilageCount(PrivilegeModel privilege) { if
	 * (privilege.getIsSelected()) return 1; else { if
	 * (privilege.getChildren().isEmpty()) return 0; else { for (PrivilegeModel
	 * child : privilege.getChildren()) { if (getPrevilageCount(child) == 1) {
	 * return 1; } }
	 *
	 * } } return 0; }
	 */
	public <T> List<T> getPage(List<T> list, Integer pageIndex, Integer pageSize) {

		if (list == null)
			return Collections.emptyList();
		else if (pageSize == null || pageSize.intValue() <= 0)
			pageSize = list.size();

		if (pageIndex == null || pageIndex.intValue() < 0)
			pageIndex = DEFAULT_PAGE_INDEX;

		int fromIndex = pageIndex.intValue() * pageSize.intValue();

		if (list.size() < fromIndex) {
			return Collections.emptyList();
		}

		return list.subList(fromIndex, Math.min(fromIndex + pageSize.intValue(), list.size()));
	}

	public int getUsersCount(String groupUuid) {
		try {
			List<UserRepresentation> userRepresentations = keyCloakBuilder.getInstance()
					.realm(keyCloakCustomConfig.getRealm()).groups().group(groupUuid).members(0, Integer.MAX_VALUE);
			return userRepresentations.size();

		} catch (NotFoundException nfe) {
			return 0;
		}
	}

	public GroupGridModel convertGroupRepresentationToGroupGridModel(GroupRepresentation groupRepresentation,GroupsResource groupsResource,
			long usersCount, List<String> clientIds,Boolean flag) {

		if (groupRepresentation == null)
			return null;

		GroupGridModel groupGridModel = new GroupGridModel();
		GroupResource group = groupsResource
				.group(groupRepresentation.getId());

		Map<String, List<String>> attributes = group.toRepresentation().getAttributes();

		groupGridModel.setId(groupRepresentation.getName());
		groupGridModel.setName(groupRepresentation.getName());

		if (attributes != null && !CollectionUtils.isEmpty(attributes.get(GroupAttribute.CREATED_AT.getLabel())))
			groupGridModel.setCreatedOn(Long.valueOf(attributes.get(GroupAttribute.CREATED_AT.getLabel()).get(0)));

		if (attributes != null && !CollectionUtils.isEmpty(attributes.get(GroupAttribute.NOTES.getLabel())))
			groupGridModel.setDescription(attributes.get(GroupAttribute.NOTES.getLabel()).get(0));

		groupGridModel.setNumberOfUsers(usersCount);
		/*
		 * if(Boolean.TRUE.equals(flag)) { List<Long> applicationAndModuleCount =
		 * getApplicationAndModuleCount(clientIds, group);
		 * groupGridModel.setNumberOfApplications((long)
		 * applicationAndModuleCount.get(0));
		 * groupGridModel.setNumberOfModules(applicationAndModuleCount.get(1)); }
		 */

		return groupGridModel;
	}



	/*
	 * private List<String> getClientIdList() { ClientsResource clients =
	 * keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).clients(
	 * ); List<ClientRepresentation> clientRepresentations = clients.findAll();
	 * clientRepresentations =
	 * aplicationServiceImpl.removeDefaultClients(clientRepresentations); return
	 * clientRepresentations.stream().filter(clientRepresentation ->
	 * clientRepresentation.isPublicClient()) .map(x ->
	 * x.getId()).collect(Collectors.toList());
	 *
	 * }
	 *
	 * private List<Long> getApplicationAndModuleCount(List<String> clientIds,
	 * GroupResource group) { Long applicationCount = (long) 0; Long moduleCount =
	 * (long) 0; RoleMappingResource roles = group.roles(); for (String clientId :
	 * clientIds) { RoleScopeResource clientLevelRoles =
	 * roles.clientLevel(clientId); long noOfRoles =
	 * clientLevelRoles.listEffective().stream().filter(x ->
	 * !x.isComposite()).count(); if (noOfRoles > 0) { applicationCount++;
	 * moduleCount = moduleCount + noOfRoles; }
	 *
	 * } return Arrays.asList(applicationCount, moduleCount);
	 *
	 * }
	 */
	private Comparator<GroupRepresentation> getGroupListRepresntSort(String sort) {

		LinkedHashMap<String, String> sortMap = UserManagementHelper.convertToMap(sort);

		List<Comparator<GroupRepresentation>> predicates = new ArrayList<Comparator<GroupRepresentation>>();

		sortMap.forEach((key, value) -> {
			log.debug(":{} has performed sort on :{} in Group detail",MDC.get(USER_ID),key);

			if (GroupRepresentationComparator.get(key) != null)
				predicates.add(GroupRepresentationComparator.get(key).getComparator(value));
		});

		return predicates.stream().reduce(Comparator::thenComparing)
				.orElse(Comparator.comparing(GroupRepresentation::getName));
	}

	private Comparator<GroupGridModel> getGroupListSort(String sort) {

		LinkedHashMap<String, String> sortMap = UserManagementHelper.convertToMap(sort);

		List<Comparator<GroupGridModel>> predicates = new ArrayList<Comparator<GroupGridModel>>();

		sortMap.forEach((key, value) -> {
			log.debug(":{} has performed sort on :{} in Group detail",MDC.get(USER_ID),key);

			if (GroupGridComparartor.get(key) != null)
				predicates.add(GroupGridComparartor.get(key).getComparator(value));
		});

		return predicates.stream().reduce(Comparator::thenComparing)
				.orElse(Comparator.comparing(GroupGridModel::getNumberOfUsers).reversed());
	}

	public boolean validateId(String groupId, String userId) {
		if (StringUtils.isEmpty(groupId) || StringUtils.isEmpty(userId))
			return false;
		else
			return true;
	}

	public boolean existsByUserId(String id) {

		try {
			UserRepresentation userRepresentation = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm())
					.users().get(id).toRepresentation();
			if (userRepresentation != null)
				return true;
			else
				return false;
		} catch (NotFoundException nfe) {
			return false;
		}

	}

	public boolean checkUserAddedToGroup(String groupId, String userName) {
		List<UserRepresentation> userRepresentations = keyCloakBuilder.getInstance()
				.realm(keyCloakCustomConfig.getRealm()).groups().group(groupId).members();

		for (UserRepresentation userRepresentation : userRepresentations)
			if (userRepresentation.getUsername().equalsIgnoreCase(userName)) {
				return true;
			}
		return false;

	}

	public void addUserToGroup(String groupUUID, String userUUID) {

		UserResource userResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).users()
				.get(userUUID);

		userResource.joinGroup(groupUUID);

	}

	public void removeUserFromGroup(String groupUUID, String userUUID) {
		UserResource userResource = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm()).users()
				.get(userUUID);

		userResource.leaveGroup(groupUUID);
	}

	public String getKeyCloakIdFromName(String name) {

		GroupRepresentation groupRepresentation = findByGroupName(name);

		if (groupRepresentation == null)
			return null;

		return groupRepresentation.getId();
	}

	public String getNameFromKeyCloakId(String id) {

		GroupRepresentation groupRepresentation = findById(id);

		if (groupRepresentation == null)
			return null;

		return groupRepresentation.getName();
	}

	public List<GroupGridModel> getGroupsListingCustomized(Integer pageIndex, Integer pageSize, String filter, String sort,
			boolean isAndFilter) {


		GroupsResource groupsResource=keyCloakBuilder.getInstance()
		.realm(keyCloakCustomConfig.getRealm()).groups();

		List<GroupRepresentation> groupRepresentations = groupsResource.groups();

		List<GroupGridModel> groupListModels = new ArrayList<>();
		groupRepresentations.stream().filter(getGroupListFilter(filter, isAndFilter))
				.forEach(groupRepresentation -> {

					groupListModels.add(convertGroupRepresentationToGroupGridModel(groupRepresentation,groupsResource,
							0, null,false));
				});

		return getPage(groupListModels.stream().sorted(getGroupListSort(sort)).collect(Collectors.toList()), pageIndex,
				pageSize);
	}



	public List<String> getGroupNames() {

		List<String> groupNames = new ArrayList<>();
		List<GroupRepresentation> groupResources = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm())
				.groups().groups(0, Integer.MAX_VALUE);

		for (GroupRepresentation groupResource : groupResources) {
			groupNames.add(groupResource.getName());
		}
		return groupNames;
	}

	public List<GroupUsersModel> getGroupUsersCount() {
		List<GroupUsersModel> groupUsersModels = new ArrayList<>();

		List<GroupRepresentation> groupResources = keyCloakBuilder.getInstance().realm(keyCloakCustomConfig.getRealm())
				.groups().groups();

		for (GroupRepresentation groupResource : groupResources) {
			GroupUsersModel groupUsersModel = new GroupUsersModel();
			groupUsersModel.setName(groupResource.getName());
			groupUsersModel.setUsersCount(Long.valueOf(getUsersCount(groupResource.getId())));

			groupUsersModels.add(groupUsersModel);

		}
		return groupUsersModels;

	}


	public Boolean getUserDetailsFromGroup(String userId,String groupName)
	{
		List<GroupRepresentation> groupRepresentations = keyCloakBuilder.getInstance()
				.realm(keyCloakCustomConfig.getRealm()).users().get(userId).groups(groupName, 0, Integer.MAX_VALUE);

		return groupRepresentations.isEmpty() ? Boolean.FALSE : Boolean.TRUE;



	}


	
	public Boolean isUserPresentInGroup(String userId,String groupName)
	{
		
		Boolean exists = false;
		List<GroupRepresentation> groupRepresentations = keyCloakBuilder.getInstance()
				.realm(keyCloakCustomConfig.getRealm()).users().get(userId).groups();
		
		for(GroupRepresentation groupRepresentation : groupRepresentations) {
			if(groupRepresentation.getId().equals(groupName)) {
				exists = true;
				break;
			}
		}

		return exists;
	}
	

}
