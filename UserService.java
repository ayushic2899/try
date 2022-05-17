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
import com.subex.ngp.usermanagement.api.UserApiDelegate;
import com.subex.ngp.usermanagement.helper.CustomHeaderConstants;
import com.subex.ngp.usermanagement.helper.PasswordHelper;
import com.subex.ngp.usermanagement.model.ChangePasswordModel;
import com.subex.ngp.usermanagement.model.ConfigurationModel;
import com.subex.ngp.usermanagement.model.DateModel;
import com.subex.ngp.usermanagement.model.IdModel;
import com.subex.ngp.usermanagement.model.UrlModel;
import com.subex.ngp.usermanagement.model.UserDetailModel;
import com.subex.ngp.usermanagement.model.UserGridModel;
import com.subex.ngp.usermanagement.model.UserLoginInfoModel;
import com.subex.ngp.usermanagement.model.UserLoginStatusModel;
import com.subex.ngp.usermanagement.model.UserModel;
import com.subex.ngp.usermanagement.model.UserSummaryModel;
import com.subex.ngp.usermanagement.service.impl.GroupServiceImpl;
import com.subex.ngp.usermanagement.service.impl.UserServiceImpl;

@Service
public class UserService implements UserApiDelegate {

	private static Logger log = LogManager.getLogger(UserService.class);

	@Value("${user-image.folder}")
	private String userImageFolder;

	@Autowired
	private UserServiceImpl userServiceImpl;
	
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
	public ResponseEntity<UserSummaryModel> getUserSummary() {

		return new ResponseEntity<>(userServiceImpl.getUserSummary(), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<UserLoginStatusModel> getLoginStatus(String interval,String userId) {

		return new ResponseEntity<>(userServiceImpl.getLoginStatus(interval,userId), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<List<UserLoginInfoModel>> getConnectedUsers(String interval, String filter,
			Integer pageSize, Integer pageIndex, String sort) {

		return new ResponseEntity<>(
				userServiceImpl.getConnectedUsers(interval, filter, pageSize, pageIndex, sort), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Void> blockUser(String userName) {

		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (id == null) {
			log.error(" {} {}",USER_ERROR, userName);
			AuditEventModel.callAuditLog("USER", "Block",
					"Blocking of user " + userName+" by "+MDC.get(USER_ID)+FAILED,
					USER_ERROR+ userName);
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}
		
		AuditEventModel.callAuditLog("USER", "Block",
				" user " + userName+" Blocked Successfully by "+MDC.get(USER_ID),
				"Blocked Successfully");
		userServiceImpl.blockUser(id);
		
		log.debug("Sucessfully blocked the user: {}",userName);

		return new ResponseEntity<>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Void> unblockUser(String userName) {

		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (id == null) {
			log.error("{} {} ",USER_ERROR, userName);
			AuditEventModel.callAuditLog("USER", "UnBlock",
					"UnBlocking of user " + userName+" by "+MDC.get(USER_ID)+FAILED,
					USER_ERROR+ userName);
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}

		userServiceImpl.unblockUser(id);
		
		AuditEventModel.callAuditLog("USER", "UnBlock",
				" user " + userName+" UnBlocked Successfully by "+MDC.get(USER_ID),
				"UnBlocked Successfully");
		
		log.debug("Sucessfully unblocked the user: {}",userName);

		return new ResponseEntity<>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Void> deleteUser(String userName) {

		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (id == null) {
			log.error("{} {}" ,USER_ERROR, userName);
			
			AuditEventModel.callAuditLog("USER", DELETE,
					"Deletion of User " + userName+" by "+MDC.get(USER_ID) +FAILED,
					USER_ERROR+userName);
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}
		if (userServiceImpl.isDeleted(id)) {
			log.error("User {} is already deleted", userName);
			
			AuditEventModel.callAuditLog("USER", DELETE,
					"Deletion of User " + userName+" by "+MDC.get(USER_ID) +FAILED,
					USER+userName+"is already deleted");
			return new ResponseEntity<>(HttpStatus.UNPROCESSABLE_ENTITY);
		}

		userServiceImpl.deleteUserByUserName(userName);
		
		AuditEventModel.callAuditLog("USER", DELETE,
				USER +userName+" Deleted Successfully by "+MDC.get(USER_ID),
				"Successfully Deleted the user");
		
		log.debug("Successfully deleted the  user: {}",userName);
		

		return new ResponseEntity<>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Void> changePassword(String userName, ChangePasswordModel changePasswordModel) {

		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (id == null) {
			log.error("{} {}",USER_ERROR, userName);
			
			AuditEventModel.callAuditLog("USER", CHANGE_PASSWORD,
					UPDATION_OF_PASSWORD_FOR + userName+" by "+MDC.get(USER_ID)+FAILED,
					USER_ERROR+ userName);
			
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}
		if (!PasswordHelper.isValid(changePasswordModel.getNewPassword())) {
			log.error("Password validation failed");
			AuditEventModel.callAuditLog("USER", CHANGE_PASSWORD,
					UPDATION_OF_PASSWORD_FOR + userName+" by "+MDC.get(USER_ID)+FAILED,
					"password not meeting the conditions");
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		try {
			userServiceImpl.changePassword(id, changePasswordModel);
		} catch (BadRequestException bre) {
			AuditEventModel.callAuditLog("USER", CHANGE_PASSWORD,
					UPDATION_OF_PASSWORD_FOR + userName+" by "+MDC.get(USER_ID)+FAILED,
					bre.getMessage());
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		
		AuditEventModel.callAuditLog("USER", CHANGE_PASSWORD,
				"Password of  User " + userName+UPDATED_SUCCESSFULLY_BY+MDC.get(USER_ID),
				"Password updated Succesfully");
		
		log.debug("Sucessfully Updated the password for user: {}",userName);
		
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<List<UserGridModel>> getUserListing(Integer pageIndex, Integer pageSize, String filter,
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
				.header(CustomHeaderConstants.X_TOTAL_COUNT, String.valueOf(userServiceImpl.getCountOfUsers(filter,groupid)))
				.body(userServiceImpl.getUserListing(pageIndex, pageSize, filter, sort, groupid, true));
	}
	
	
	@Override
	public ResponseEntity<List<UserGridModel>> getCustomizedUserListing(Integer pageIndex, Integer pageSize,
			String filter, String sort) {

		return ResponseEntity.ok()
				.header(CustomHeaderConstants.X_TOTAL_COUNT, String.valueOf(userServiceImpl.getCountOfUsers(filter, null)))
				.body(userServiceImpl.getUserListingCustomized(pageIndex, pageSize, filter, sort, true));
	}

	@Override
	public ResponseEntity<IdModel> createUser(UserModel userModel)
	{
		IdModel invalidUserDetails=new IdModel();
		invalidUserDetails.setValidEmail(Boolean.TRUE);
		invalidUserDetails.setValidUserName(Boolean.TRUE);

		if (!userServiceImpl.validate(userModel)) {
			log.error("User validation failed");
			
			AuditEventModel.callAuditLog("USER", CREATE,
					CREATION_OF_USER + userModel.getUserName() + " by " + MDC.get(USER_ID) + FAILED,
					"please fill all the Mandatory Fields for  " + userModel.getUserName());
			
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		if (!PasswordHelper.isValid(userModel.getPassword()) && userModel.getIdentityProviderLink() == null) {
			log.error("Password validation failed");

			AuditEventModel.callAuditLog("USER", CREATE,
					CREATION_OF_USER + userModel.getUserName() + " by " + MDC.get(USER_ID) + FAILED,
					"Password validation failed for " + userModel.getUserName());
			
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		if (userServiceImpl.existsByUserName(userModel.getUserName())) {
			log.error("User already exists with username: {}", userModel.getUserName());
			AuditEventModel.callAuditLog("USER", CREATE,
					CREATION_OF_USER + userModel.getUserName()+" by "+MDC.get(USER_ID) +FAILED,
					"User already exists with username: "+userModel.getUserName());
			invalidUserDetails.setValidUserName(Boolean.FALSE);
		}
			
		if(userServiceImpl.existsByEmail(userModel.getEmail()))
		{
			log.error("User already exists with email: {}", userModel.getEmail());
			AuditEventModel.callAuditLog("USER", CREATE,
					CREATION_OF_USER + userModel.getUserName()+" by "+MDC.get(USER_ID) +FAILED,
					"User already exists with email: "+userModel.getUserName());
			invalidUserDetails.setValidEmail(Boolean.FALSE);
		}
		if(invalidUserDetails.getValidEmail().equals(Boolean.FALSE) || invalidUserDetails.getValidUserName().equals(Boolean.FALSE))
			return new ResponseEntity<>(invalidUserDetails,HttpStatus.OK);
		

		IdModel idModel = userServiceImpl.createUser(userModel);

		if (idModel == null)
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		
		idModel.setValidEmail(Boolean.TRUE);
		idModel.setValidUserName(Boolean.TRUE);
		
		AuditEventModel.callAuditLog("USER", CREATE,
				USER + userModel.getUserName()+" Created Successfully by "+MDC.get(USER_ID),
				"Successfully Created the user");
		
		log.debug("Created user successfully:{}",userModel.getUserName());

		return new ResponseEntity<>(idModel.id(userServiceImpl.getUserNameFromKeyCloakId(idModel.getId())),
				HttpStatus.CREATED);
	}

	@Override
	public ResponseEntity<String> updateUser(String userName, UserModel userModel) 
	{
		
		JsonObject jsonObject=new JsonObject();
		
		jsonObject.addProperty(VALIDEMAIL, Boolean.TRUE);
		
		jsonObject.addProperty(VALIDUSER, Boolean.TRUE);

		
		jsonObject.addProperty(ERROR, "updated user successfully");
		
		
		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (id == null) {
			log.error("{} {} " ,USER_ERROR, userName);
			
			AuditEventModel.callAuditLog("USER", "Edit",
					FAILED_TO_UPDATE_USER + userModel.getUserName()+" by"+MDC.get(USER_ID),
					USER_ERROR + userName);
			
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}
		if (!userServiceImpl.validate(userModel)) {
			log.error("User validation failed");
			AuditEventModel.callAuditLog("USER", "Edit",
					FAILED_TO_UPDATE_USER + userModel.getUserName()+" by "+MDC.get(USER_ID),
					"User validation failed ");
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		if (!userServiceImpl.existsByIdWithName(id, userModel.getUserName())) {
			
			log.error("There already exists another user with username: {}" , userModel.getUserName());
			
			jsonObject.addProperty(ERROR, "user with id  already exists");
			
			jsonObject.addProperty(VALIDUSER, Boolean.FALSE);


			
			AuditEventModel.callAuditLog("USER", "Edit",
					FAILED_TO_UPDATE_USER + userModel.getUserName()+" by "+MDC.get(USER_ID),
					"There already exists another user with username: ");

		}
		
		if (!userServiceImpl.existsByIdWithEmail(id, userModel.getEmail()))
		{
			log.error("There already exists another user with email: {}", userModel.getEmail());

			AuditEventModel.callAuditLog("USER", "Edit",
					FAILED_TO_UPDATE_USER + userModel.getUserName() + " by " + MDC.get(USER_ID),
					"There already exists another user with email: ");
			jsonObject.addProperty(VALIDEMAIL, Boolean.FALSE);
			jsonObject.addProperty(ERROR, "user with email already exists");


		}
		
		if(Boolean.FALSE.equals(jsonObject.get(VALIDEMAIL).getAsBoolean()) || Boolean.FALSE.equals(jsonObject.get(VALIDUSER).getAsBoolean()))
		{
			return new ResponseEntity<>(jsonObject.toString(),HttpStatus.OK);

			
		}

		try {
			userServiceImpl.updateUser(id, userModel);
			
		} catch (ClientErrorException cee) {
			AuditEventModel.callAuditLog("USER", "Edit",
					FAILED_TO_UPDATE_USER + userModel.getUserName()+" by "+MDC.get(USER_ID),
					"Error while connecting with keycloak: ");
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}
		AuditEventModel.callAuditLog("USER", "Edit",
				USER + userModel.getUserName()+UPDATED_SUCCESSFULLY_BY+MDC.get(USER_ID),
				"Successfully Updated the user");
		log.debug("Updated the user Successfully: {}",userModel.getUserName());
		return new ResponseEntity<>(jsonObject.toString(),HttpStatus.OK);
	}

	@Override
	public ResponseEntity<UserModel> getUser(String userName) {

		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (id == null) {
			log.error("{} {}",USER_ERROR, userName);
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}

		UserModel userModel = userServiceImpl.getUser(id);

		return new ResponseEntity<>(userModel, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Void> uploadProfilePicture(String userName, MultipartFile multipartFile) {

		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		File folder = new File(userImageFolder);

		if (id == null) {
			log.error("{} {}" ,USER_ERROR, userName);
			AuditEventModel.callAuditLog("USER", UPLOAD_PROFILE_PICTURE,
					"Uploading of profile picture for " + userName+ " by " +MDC.get(USER_ID) +FAILED ,
					USER_ERROR + userName);
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		if (!folder.exists() || !folder.isDirectory()) {
			try {
				Files.createDirectories(folder.toPath());
				log.warn("{} {} . Hence created!" ,FOLDER_ERROR ,userImageFolder);
			} catch (IOException e) {
				log.error("Failed to create folder: {} {} " , userImageFolder, e);
				
				AuditEventModel.callAuditLog("USER", UPLOAD_PROFILE_PICTURE,
						"Uploading of profile picture for " + userName+ " by " +MDC.get(USER_ID) +FAILED ,
						"Failed to create folder: " + userImageFolder);
			}
		}

		userServiceImpl.uploadProfilePicture(userName, multipartFile, userImageFolder);
		
		AuditEventModel.callAuditLog("USER", UPLOAD_PROFILE_PICTURE,
				"Profile picture of  User " + userName+UPDATED_SUCCESSFULLY_BY+MDC.get(USER_ID),
				"Profile picture  updated Succesfully");
		
		log.debug("Sucessfully Updated the Profile picture for user: {}",userName);


		return new ResponseEntity<>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<UrlModel> getProfilePicture(String userName) {

		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		File folder = new File(userImageFolder);

		if (id == null) {
			log.error("{} {}" ,USER_ERROR, userName);
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		if (!folder.exists() || !folder.isDirectory()) {
			log.error("{} {}",FOLDER_ERROR , userImageFolder);
			return new ResponseEntity<>(HttpStatus.UNPROCESSABLE_ENTITY);
		}

		UrlModel urlModel = userServiceImpl.getProfilePicture(userName, userImageFolder);

		if (urlModel == null)
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);

		return new ResponseEntity<>(urlModel, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Void> deleteProfilePicture(String userName) {
		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		File folder = new File(userImageFolder);

		if (id == null) {
			log.error("{} {}", USER_ERROR, userName);
			
			AuditEventModel.callAuditLog("USER", DELETE_PROFILE_PICTURE,
					DELETING_OF_PROFILE_PICTURE_FOR + userName+ " by " +MDC.get(USER_ID) +FAILED ,
					USER_ERROR + userName);
			
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		if (!folder.exists() || !folder.isDirectory()) {
			log.error("{} {}",FOLDER_ERROR , userImageFolder);
			
			AuditEventModel.callAuditLog("USER", DELETE_PROFILE_PICTURE,
					DELETING_OF_PROFILE_PICTURE_FOR + userName + " by " + MDC.get(USER_ID) + FAILED,
					FOLDER_ERROR + userImageFolder);
			
			return new ResponseEntity<>(HttpStatus.UNPROCESSABLE_ENTITY);
		}
		Boolean found = userServiceImpl.deleteProfilePicture(userName, userImageFolder);

		if (found == null) {
			AuditEventModel.callAuditLog("USER", DELETE_PROFILE_PICTURE,
					DELETING_OF_PROFILE_PICTURE_FOR + userName+ " by " +MDC.get(USER_ID) +FAILED ,
					 "User Image Not Found");
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}

		if (!found) {
			log.error("Unable to delete file, internal server error.");
			AuditEventModel.callAuditLog("USER", DELETE_PROFILE_PICTURE,
					DELETING_OF_PROFILE_PICTURE_FOR + userName+ " by " +MDC.get(USER_ID) +FAILED ,
					 "Unable to delete file, internal server error. ");
			return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
		}
		AuditEventModel.callAuditLog("USER", DELETE_PROFILE_PICTURE,
				"Profile picture of  User" + userName+"Deleted Successfully by"+MDC.get(USER_ID),
				"Profile picture  Deleted Succesfully");
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<UserDetailModel> getUserDetail(String userName) {

		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (id == null) {
			log.error("{} {}" ,USER_ERROR, userName);
			AuditEventModel.callAuditLog("USER", "View",
					"Failed to view User " + userName+ " by "+MDC.get(USER_ID),
					USER_ERROR + userName);
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}
		
		

		return new ResponseEntity<>(userServiceImpl.getUserDetail(id), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Void> extendAccountExpiryDate(String userName, DateModel dateModel) {

		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (id == null) {
			log.error("{} {}",USER_ERROR, userName);
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}
		if (dateModel.getDate() < new Date().getTime()) {
			log.error("Account expiry date cannot be lesser than current date");
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}

		userServiceImpl.extendAccountExpiryDate(id, dateModel);
		
		AuditEventModel.callAuditLog("USER", " Change expiry",
				"Expiry Date of  User" + userName+"Updated Successfully by"+MDC.get(USER_ID),
				"Expiry Date updated Succesfully");
		
		log.debug("Sucessfully Updated the expiry date for user: {}",userName);

		return new ResponseEntity<>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Void> addUserConfiguration(ConfigurationModel configurationModel) {

		if (StringUtils.isEmpty(configurationModel.getKey())) {
			log.error("Configuration key cannot be null or empty");
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}

		userServiceImpl.addUserConfiguration(configurationModel);

		return new ResponseEntity<>(HttpStatus.OK);
	}

	@Override
	public ResponseEntity<ConfigurationModel> getConfiguration(String key) {

		if (StringUtils.isEmpty(key)) {
			log.error("Configuration key cannot be null or empty");
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}

		return new ResponseEntity<>(userServiceImpl.getConfiguration(key), HttpStatus.OK);
	}
	
	@Override
    public ResponseEntity<Boolean> getIsFederated() {
        Boolean successFlag = userServiceImpl.getIsFederated();
        return new ResponseEntity<>(successFlag, HttpStatus.OK); 
    }
	
	@Override
	public ResponseEntity<Integer> getUsersCount(String filter) {
        return new ResponseEntity<>(userServiceImpl.getUsersCount(filter), HttpStatus.OK);
    }
	
	@Override
	public ResponseEntity<List<String>> getUserGroupNames(String userName) {
		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (id == null) {
			log.error("{} {}", USER_ERROR, userName);
			AuditEventModel.callAuditLog("USER", "View", "Failed to view User " + userName + " by " + MDC.get(USER_ID),
					USER_ERROR + userName);
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}

		return new ResponseEntity<>(userServiceImpl.getUserGroupNames(id), HttpStatus.OK);

	}
	
	@Override
	public ResponseEntity<List<String>> getCustomizedUserGroupNames(String userName) {
		String id = userServiceImpl.getKeyCloakIdFromUserName(userName);

		if (id == null) {
			log.error("{} {}", USER_ERROR, userName);
			AuditEventModel.callAuditLog("USER", "View", "Failed to view User " + userName + " by " + MDC.get(USER_ID),
					USER_ERROR + userName);
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}

		return new ResponseEntity<>(userServiceImpl.getUserGroupNames(id), HttpStatus.OK);

	}

	@Override
	public ResponseEntity<List<String>> getUserNames() {
		return new ResponseEntity<>(userServiceImpl.getUserNames(), HttpStatus.OK);

	}

}
