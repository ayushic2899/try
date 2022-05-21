package com.subex.ngp.usermanagement.service;

import java.util.List;
import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.subex.ngp.audit.trail.lib.AuditEventModel;
import com.subex.ngp.usermanagement.api.GroupApiDelegate;
import com.subex.ngp.usermanagement.exception.UserManagementConflict;
import com.subex.ngp.usermanagement.exception.UserManagementErrorCode;
import com.subex.ngp.usermanagement.exception.UserManagementServerError;
import com.subex.ngp.usermanagement.helper.CustomHeaderConstants;
import com.subex.ngp.usermanagement.model.ApplicationModel;
import com.subex.ngp.usermanagement.model.GroupDetailModel;
import com.subex.ngp.usermanagement.model.GroupGridModel;
import com.subex.ngp.usermanagement.model.GroupModel;
import com.subex.ngp.usermanagement.model.GroupUsersModel;
import com.subex.ngp.usermanagement.model.IdModel;
import com.subex.ngp.usermanagement.service.impl.GroupServiceImpl;
import com.subex.ngp.usermanagement.service.impl.UserServiceImpl;

@Service
public class GroupService implements GroupApiDelegate {

	private static Logger log = LogManager.getLogger(GroupService.class);

	@Autowired
	private GroupServiceImpl groupServiceImpl;

	@Autowired
	private UserServiceImpl userServiceImpl;
	
	private static final String GROUP_ERROR="No group exists with name: ";
	
	private static final String GROUP_VALIDATION_ERROR="Group validation failed";
	
	private static final String GROUP="GROUP";
	
	private static final String CREATE_GROUP="CREATE";
	
	private static final String USER_ID="userId";
	
	private static final String CREATION_OF_GROUP = "Creation of  Group ";

	private static final String FAILED = " Failed ";
	
	private static final String GROUP_SMALL = "Group ";
	
	private static final String DELETE = "Delete";

	private static final String UPDATION_OF_GROUP ="Updation of  Group ";
	
	private static final String ADD_USER_TO_GROUP = "Add User to group";
	@Override
	public ResponseEntity<IdModel> createGroup(GroupModel groupModel) {

		if (!groupServiceImpl.validate(groupModel)) {
			log.error(GROUP_VALIDATION_ERROR);
			AuditEventModel.callAuditLog(GROUP, CREATE_GROUP,
					CREATION_OF_GROUP + groupModel.getName()+" by "+MDC.get(USER_ID) +FAILED,
					GROUP_VALIDATION_ERROR);
			
			return new ResponseEntity<IdModel>(HttpStatus.BAD_REQUEST);
		}

		if (groupServiceImpl.existsByGroupName(groupModel.getName())) {
			log.error("Group already exists with name: {}", groupModel.getName());
			AuditEventModel.callAuditLog(GROUP, CREATE_GROUP,
					CREATION_OF_GROUP + groupModel.getName()+" by "+MDC.get(USER_ID) +FAILED,
					"Group already exists with name: "+groupModel.getName());
			
			return new ResponseEntity<IdModel>(HttpStatus.UNPROCESSABLE_ENTITY);
		}

		IdModel idModel = groupServiceImpl.createGroup(groupModel);

		if (idModel == null) {
			
			AuditEventModel.callAuditLog(GROUP, CREATE_GROUP,
					CREATION_OF_GROUP + groupModel.getName()+" by "+MDC.get(USER_ID) +FAILED,
					"error while contacting the keycloak");
			
			log.error("Creation of Group failed due to keycloak issue: {}",groupModel.getName());
			
			return new ResponseEntity<IdModel>(HttpStatus.BAD_REQUEST);
		}
		
		AuditEventModel.callAuditLog(GROUP, CREATE_GROUP,
				GROUP_SMALL + groupModel.getName()+" Created Successfully by "+MDC.get(USER_ID),
				"Successfully Created the Group");
		
		log.debug("Group created Successfully with name: {}",groupModel.getName());


		return new ResponseEntity<>(idModel.id(groupServiceImpl.getNameFromKeyCloakId(idModel.getId())),
				HttpStatus.CREATED);
	}

	@Override
	public ResponseEntity<Void> deleteGroup(String name) {

		String id = groupServiceImpl.getKeyCloakIdFromName(name);

		if (id == null) {
			log.error("{} {}",GROUP_ERROR, name);
			AuditEventModel.callAuditLog(GROUP, DELETE,
					" Deletion of  Group " + name+" by "+MDC.get(USER_ID) +FAILED,
					GROUP_ERROR + name);
			
			return new ResponseEntity<Void>(HttpStatus.NOT_FOUND);
		}
		if (groupServiceImpl.isDeleted(id)) {
			log.error("Group {} is already deleted", name);
			
			AuditEventModel.callAuditLog(GROUP, DELETE,
					" Deletion of  Group " + name+" by "+MDC.get(USER_ID) +FAILED,
					"Group  is already deleted " + name);
			
			return new ResponseEntity<Void>(HttpStatus.UNPROCESSABLE_ENTITY);
		}

		groupServiceImpl.delete(id);
		
		AuditEventModel.callAuditLog(GROUP, DELETE,
				GROUP_SMALL +name+" Deleted Successfully by "+MDC.get(USER_ID),
				"Successfully Deleted the group");
		
		log.debug("Deleted Group {}  successfully",name);

		return new ResponseEntity<Void>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<List<ApplicationModel>> getGroupApplications(String name) {
		String id = groupServiceImpl.getKeyCloakIdFromName(name);

		if (id == null) {
			log.error("{} {}",GROUP_ERROR, name);
			return new ResponseEntity<List<ApplicationModel>>(HttpStatus.NOT_FOUND);
		} else
			return new ResponseEntity<List<ApplicationModel>>(groupServiceImpl.getGroupApplications(id), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<GroupDetailModel> getGroupDetails(String name) {

		String id = groupServiceImpl.getKeyCloakIdFromName(name);

		if (id == null) {
			log.error("{} {}",GROUP_ERROR, name);
			
			AuditEventModel.callAuditLog(GROUP, "View",
					"Failed to view Group " + name+ " by "+MDC.get(USER_ID),
					GROUP_ERROR + name);
			return new ResponseEntity<GroupDetailModel>(HttpStatus.NOT_FOUND);
		} else {
			Optional<GroupDetailModel> groupDetailsOpt = groupServiceImpl.getGroupDetails(id);
			if(groupDetailsOpt.isPresent())
				return new ResponseEntity<GroupDetailModel>(groupDetailsOpt.get(), HttpStatus.OK);
			else 
				return new ResponseEntity<GroupDetailModel>(new GroupDetailModel(), HttpStatus.OK);
		}

	}

	@Override
	public ResponseEntity<Void> updateGroup(String name, GroupModel groupModel) {

		String id = groupServiceImpl.getKeyCloakIdFromName(name);

		if (id == null) {
			log.error("{} {}",GROUP_ERROR, name);
			AuditEventModel.callAuditLog(GROUP, "Edit",
					UPDATION_OF_GROUP + groupModel.getName() + " by " + MDC.get(USER_ID) + FAILED,
					GROUP_ERROR + name);
			return new ResponseEntity<Void>(HttpStatus.NOT_FOUND);
		}
		if (!groupServiceImpl.validate(groupModel)) {
			log.error(GROUP_VALIDATION_ERROR);
			AuditEventModel.callAuditLog(GROUP, "Edit",
					UPDATION_OF_GROUP + groupModel.getName() + " by " + MDC.get(USER_ID) + FAILED,
					GROUP_VALIDATION_ERROR);
			return new ResponseEntity<Void>(HttpStatus.BAD_REQUEST);
		}
		if (!groupServiceImpl.existsByIdWithName(id, groupModel.getName())) {
			log.error("There already exists another group with name: {}" , groupModel.getName());
			AuditEventModel.callAuditLog(GROUP, "Edit",
					UPDATION_OF_GROUP + groupModel.getName() + " by " + MDC.get(USER_ID) + FAILED,
					"There already exists another group with name: " + groupModel.getName());
			return new ResponseEntity<Void>(HttpStatus.UNPROCESSABLE_ENTITY);
		}

		groupServiceImpl.updateGroup(id, groupModel);
		
		AuditEventModel.callAuditLog(GROUP, "Edit",
				GROUP_SMALL + groupModel.getName()+" Updated Successfully by "+MDC.get(USER_ID),
				"Successfully Updated the Group");
		
		log.debug("Successfully updated the group:{}",groupModel.getName());

		return new ResponseEntity<Void>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<List<GroupGridModel>> getGroupsList(Integer pageIndex, Integer pageSize, String filter,
			String sort) {

		return ResponseEntity.ok()
				.header(CustomHeaderConstants.X_TOTAL_COUNT, String.valueOf(groupServiceImpl.getCountOfGroups(filter)))
				.body(groupServiceImpl.getGroupsListing(pageIndex, pageSize, filter, sort, true));
	}
	
	@Override
	public ResponseEntity<List<GroupGridModel>> getCustomizedGroupListing(Integer pageIndex, Integer pageSize, String filter,
			String sort) {

		return ResponseEntity.ok()
				.header(CustomHeaderConstants.X_TOTAL_COUNT, String.valueOf(groupServiceImpl.getCountOfGroups(filter)))
				.body(groupServiceImpl.getGroupsListingCustomized(pageIndex, pageSize, filter, sort, true));
	}

	@Override
	public ResponseEntity<Void> addUserToGroup(String groupName, String userName) {

		if (!groupServiceImpl.validateId(groupName, userName)) {
			log.error("Group name and User name cannot be null or empty");
			return new ResponseEntity<Void>(HttpStatus.BAD_REQUEST);
		}

		String groupId = groupServiceImpl.getKeyCloakIdFromName(groupName);
		String userId = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (groupId == null) {
			log.error("{} {}", GROUP_ERROR, groupName);
			
			AuditEventModel.callAuditLog(GROUP, ADD_USER_TO_GROUP,
					"Failed to Add User " +userName+"  to group "+ groupName+" by "+MDC.get(USER_ID),
					GROUP_ERROR+ groupName);

			return new ResponseEntity<Void>(HttpStatus.NOT_FOUND);
		}
		if (userId == null) {
			
			log.error("No user exists with username:{} " , userName);
			AuditEventModel.callAuditLog(GROUP, ADD_USER_TO_GROUP,
					"Failed to Add User " +userName+"  to group "+ groupName+" by "+MDC.get(USER_ID),
					"No user exists with username: "+ userName);
			return new ResponseEntity<Void>(HttpStatus.NOT_FOUND);
		}
		
		
		Boolean userAlreadyExists=groupServiceImpl.getUserDetailsFromGroup(userId,groupName);
		
		if(Boolean.TRUE.equals(userAlreadyExists))
		{
			log.error("user is Already part of the Group {} ", groupName);
			
			
		
			throw new UserManagementConflict(UserManagementErrorCode.ERROR40906,userName,groupName);
			
			
		}

		groupServiceImpl.addUserToGroup(groupId, userId);
		
		AuditEventModel.callAuditLog(GROUP, ADD_USER_TO_GROUP,
				"User " +userName+" has been added Successfully to group "+ groupName+" by "+MDC.get(USER_ID),
				"Successfully added the user to group");
		
		log.debug("Added User to the Group successfully: {}",groupName);

		return new ResponseEntity<Void>(HttpStatus.OK);
	}
	
	@Override
	public ResponseEntity<Long> getGroupsCount(String filter) {
        return new ResponseEntity<>(groupServiceImpl.getCountOfGroups(filter), HttpStatus.OK);
    }
	
	@Override
	public ResponseEntity<List<String>> getGroupNames()
	{
		try {
			return new ResponseEntity<>(groupServiceImpl.getGroupNames(), HttpStatus.OK);
		} catch (Exception e) {
			log.error("unable to fetch the group Names", e);
			throw new UserManagementServerError();
		}
	}
	
	@Override
	public ResponseEntity<List<GroupUsersModel>> getGroupUsersCount() {
		return new ResponseEntity<>(groupServiceImpl.getGroupUsersCount(), HttpStatus.OK);
	}
	
	@Override
	public ResponseEntity<ApplicationModel> getGroupApplicationForClient(String name,String clientName) {
		String id = groupServiceImpl.getKeyCloakIdFromName(name);
		try {
			return new ResponseEntity<ApplicationModel>(groupServiceImpl.getGroupApplicationForClient(id, clientName), HttpStatus.OK);
		} catch (Exception e) {
			e.printStackTrace();
			return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	public ResponseEntity<Void> removeUserFromGroup(String groupId, String userId) {
		if (!groupServiceImpl.validateId(groupId, userId)) {
			log.error("Group name and User name cannot be null or empty");
			return new ResponseEntity<Void>(HttpStatus.BAD_REQUEST);
		}

		String groupUUId = groupServiceImpl.getKeyCloakIdFromName(groupId);
		String userUUId = userServiceImpl.getKeyCloakIdFromUserName(userId);

		if (groupId == null) {
			log.error("{} {}", GROUP_ERROR, groupId);
			
			AuditEventModel.callAuditLog(GROUP, ADD_USER_TO_GROUP,
					"Failed to Remove User " +userId+"  from group "+ groupId+" by "+MDC.get(USER_ID),
					GROUP_ERROR+ groupId);

			return new ResponseEntity<Void>(HttpStatus.NOT_FOUND);
		}
		if (userId == null) {
			
			log.error("No user exists with username:{} " , userId);
			AuditEventModel.callAuditLog(GROUP, ADD_USER_TO_GROUP,
					"Failed to Remove User " +userId+"  from group "+ groupId+" by "+MDC.get(USER_ID),
					"No user exists with username: "+ userId);
			return new ResponseEntity<Void>(HttpStatus.NOT_FOUND);
		}
		
		
		Boolean userIsPresentInGroup=groupServiceImpl.isUserPresentInGroup(userUUId,groupUUId);
		
		if(Boolean.TRUE.equals(userIsPresentInGroup))
		{
			groupServiceImpl.removeUserFromGroup(groupUUId, userUUId);
		} else {
			log.error("User with username:{} is no a part of the group with groupname:{}" , userId, groupId);
			AuditEventModel.callAuditLog(GROUP, ADD_USER_TO_GROUP,
					"Failed to Remove User " +userId+"  from group "+ groupId+" by "+MDC.get(USER_ID),
					"User with username: "+userId + " is no a part of the group with groupname: " +groupId);
			return new ResponseEntity<Void>(HttpStatus.NOT_FOUND);
		}
		
		AuditEventModel.callAuditLog(GROUP, ADD_USER_TO_GROUP,
				"User " +userId+" has been removed Successfully from the group "+ groupId+" by "+MDC.get(USER_ID),
				"Successfully removed the user from the group");
		
		log.debug("Removed User From the Group successfully: {}",groupId);

		return new ResponseEntity<Void>(HttpStatus.OK);

	}

	@Override
	public ResponseEntity<List<ApplicationModel>> getGroupApplicationForParent(String name,String parentName) {
		String id = groupServiceImpl.getKeyCloakIdFromName(name);
		if (id == null) {
			log.error("{} {}",GROUP_ERROR, name);
			return new ResponseEntity<List<ApplicationModel>>(HttpStatus.NOT_FOUND);
		} else
			try {
				return new ResponseEntity<List<ApplicationModel>>(groupServiceImpl.getGroupApplicationForParent(id, parentName), HttpStatus.OK);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
			}

	}
}
