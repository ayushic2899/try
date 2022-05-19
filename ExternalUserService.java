package com.subex.ngp.usermanagement.service;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Date;
import java.util.List;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.ClientErrorException;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.google.gson.JsonObject;
import com.subex.ngp.audit.trail.lib.AuditEventModel;
import com.subex.ngp.usermanagement.api.UserExternalApiDelegate;
import com.subex.ngp.usermanagement.helper.CustomHeaderConstants;
 
import com.subex.ngp.usermanagement.model.DateModel;
import com.subex.ngp.usermanagement.model.IdModel;
import com.subex.ngp.usermanagement.model.UrlModel;
import com.subex.ngp.usermanagement.model.UserDetailModel;
import com.subex.ngp.usermanagement.model.ExternalUserGridModel;
import com.subex.ngp.usermanagement.service.impl.GroupServiceImpl;
import com.subex.ngp.usermanagement.model.ExternalUserModel;
import com.subex.ngp.usermanagement.service.impl.ExternalUserServiceImpl;

@Service
public class ExternalUserService implements UserExternalApiDelegate {

	private static Logger log = LogManager.getLogger(ExternalUserService.class);

	@Value("${user-image.folder}")
	private String userImageFolder;

	@Autowired
	private ExternalUserServiceImpl externalUserServiceImpl;
	
	@Autowired
	private ExternalUserModel externalUserModel;
	
	
	@Autowired
	private GroupServiceImpl groupServiceImpl;

	private static final String USER_ERROR="No user exists with name: ";
	
	private static final String USER_ID="userId";
	
	private static final String FAILED = " failed ";
	
	private static final String DELETE = "Delete";
	
	private static final String USER = "User ";
	
	private static final String CHANGE_PASSWORD = "Change Password";
	
	private static final String UPDATION_OF_PASSWORD_FOR = "Updation of Password for ";
	
	private static final String UPDATED_SUCCESSFULLY_BY = " Updated Successfully by ";
	
	private static final String CREATE = "Create";
	
	private static final String VALIDEMAIL="validEmail";
	
	private static final String VALIDUSER="validUser";
	
	private static final String ERROR="message";
	
	private static final String CREATION_OF_USER = "Creation of User ";
	
	private static final String FAILED_TO_UPDATE_USER = "Failed to Update User ";
	
	private static final String UPLOAD_PROFILE_PICTURE = "Upload Profile Picture";
	
	private static final String DELETE_PROFILE_PICTURE = "Delete Profile Picture";
	
	private static final String DELETING_OF_PROFILE_PICTURE_FOR =  "Deleting of profile picture for ";
	
	private static final String FOLDER_ERROR="No folder exists: ";
	 

	@Override
	public ResponseEntity<List<ExternalUserGridModel>> getExternalUserListing(Integer pageIndex, Integer pageSize, String filter,
			String sort, String groupName) 
	{
		String groupid=null;
		
		if(groupName!=null)
		{
			groupid=groupServiceImpl.getKeyCloakIdFromName(groupName);
			if(groupid==null)
			{
				log.error("No group exists in user-mangement with name {}",groupName);
				return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
			}
		}

		return ResponseEntity.ok()
				.header(CustomHeaderConstants.X_TOTAL_COUNT, String.valueOf(externalUserServiceImpl.getCountOfUsers(filter,groupid)))
				.body(externalUserServiceImpl.getExternalUserListing(pageIndex, pageSize, filter, sort, groupid, true));
	}
	
	 

	@Override
	public ResponseEntity<IdModel> createExternalUser(ExternalUserModel externalUserModel)
	{
		IdModel invalidUserDetails=new IdModel();
		invalidUserDetails.setValidEmail(Boolean.TRUE);
		invalidUserDetails.setValidUserName(Boolean.TRUE);

		if (!externalUserServiceImpl.validate(externalUserModel)) {
			log.error("User validation failed");
			
			AuditEventModel.callAuditLog("USER", CREATE,
					CREATION_OF_USER + externalUserModel.getUserName() + " by " + MDC.get(USER_ID) + FAILED,
					"please fill all the Mandatory Fields for  " + externalUserModel.getUserName());
			
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		 
		if (externalUserServiceImpl.existsByUserName(externalUserModel.getUserName())) {
			log.error("User already exists with username: {}", externalUserModel.getUserName());
			AuditEventModel.callAuditLog("USER", CREATE,
					CREATION_OF_USER + externalUserModel.getUserName()+" by "+MDC.get(USER_ID) +FAILED,
					"User already exists with username: "+externalUserModel.getUserName());
			invalidUserDetails.setValidUserName(Boolean.FALSE);
		}
			
		if(externalUserServiceImpl.existsByEmail(externalUserModel.getEmail()))
		{
			log.error("User already exists with email: {}", externalUserModel.getEmail());
			AuditEventModel.callAuditLog("USER", CREATE,
					CREATION_OF_USER + externalUserModel.getUserName()+" by "+MDC.get(USER_ID) +FAILED,
					"User already exists with email: "+externalUserModel.getUserName());
			invalidUserDetails.setValidEmail(Boolean.FALSE);
		}
		if(invalidUserDetails.getValidEmail().equals(Boolean.FALSE) || invalidUserDetails.getValidUserName().equals(Boolean.FALSE))
			return new ResponseEntity<>(invalidUserDetails,HttpStatus.OK);
		

		IdModel idModel = externalUserServiceImpl.createUser(externalUserModel);

		if (idModel == null)
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		
		idModel.setValidEmail(Boolean.TRUE);
		idModel.setValidUserName(Boolean.TRUE);
		
		AuditEventModel.callAuditLog("USER", CREATE,
				USER + externalUserModel.getUserName()+" Created Successfully by "+MDC.get(USER_ID),
				"Successfully Created the user");
		
		log.debug("Created user successfully:{}",externalUserModel.getUserName());

		return new ResponseEntity<>(idModel.id(externalUserServiceImpl.getUserNameFromKeyCloakId(idModel.getId())),
				HttpStatus.CREATED);
	}
 
 

	 
 
 
	
	  
 

}

