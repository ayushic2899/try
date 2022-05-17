package com.subex.ngp.usermanagement.service.impl;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.keycloak.admin.client.resource.RealmResource;
import org.keycloak.admin.client.resource.UserResource;
import org.keycloak.admin.client.resource.UsersResource;
import org.keycloak.representations.idm.ComponentRepresentation;
import org.keycloak.representations.idm.CredentialRepresentation;
import org.keycloak.representations.idm.EventRepresentation;
import org.keycloak.representations.idm.FederatedIdentityRepresentation;
import org.keycloak.representations.idm.GroupRepresentation;
import org.keycloak.representations.idm.IdentityProviderRepresentation;
import org.keycloak.representations.idm.UserRepresentation;
import org.keycloak.util.JsonSerialization;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import com.subex.common.settings.DateTimeApiAuth;
import com.subex.ngp.audit.trail.lib.AuditEventModel;
import com.subex.ngp.usermanagement.enums.Intervals;
import com.subex.ngp.usermanagement.enums.KeyCloakEventType;
import com.subex.ngp.usermanagement.enums.LoginStatus;
import com.subex.ngp.usermanagement.enums.UserGridComparator;
import com.subex.ngp.usermanagement.enums.UserRepresentationComparator;
import com.subex.ngp.usermanagement.enums.UserStatus;
import com.subex.ngp.usermanagement.exception.ChangePasswordException;
import com.subex.ngp.usermanagement.helper.CalendarUtil;
import com.subex.ngp.usermanagement.helper.EmailValidator;
import com.subex.ngp.usermanagement.helper.PasswordHelper;
import com.subex.ngp.usermanagement.helper.UserManagementHelper;
import com.subex.ngp.usermanagement.keycloak.KeyCloakBuilder;
import com.subex.ngp.usermanagement.keycloak.NgpSession;
import com.subex.ngp.usermanagement.keycloak.configuration.KeycloakCustomConfig;
import com.subex.ngp.usermanagement.keycloak.helper.UserAttribute;
import com.subex.ngp.usermanagement.keycloak.helper.UserModelToUserRepresentation;
import com.subex.ngp.usermanagement.keycloak.helper.UserRepresentationToUserDetailModel;
import com.subex.ngp.usermanagement.keycloak.helper.UserRepresentationToUserGridModel;
import com.subex.ngp.usermanagement.keycloak.helper.UserRepresentationToUserModel;
import com.subex.ngp.usermanagement.model.ChangePasswordModel;
import com.subex.ngp.usermanagement.model.ConfigurationModel;
import com.subex.ngp.usermanagement.model.DateModel;
import com.subex.ngp.usermanagement.model.IdModel;
import com.subex.ngp.usermanagement.model.SearchResultItemModel;
import com.subex.ngp.usermanagement.model.UrlModel;
import com.subex.ngp.usermanagement.model.UserDetailModel;
import com.subex.ngp.usermanagement.model.ExternalUserGridModel;
import com.subex.ngp.usermanagement.model.UserLoginInfoModel;
import com.subex.ngp.usermanagement.model.UserLoginStatusModel;
import com.subex.ngp.usermanagement.model.ExternalUserModel;
import com.subex.ngp.usermanagement.model.UserSummaryModel;

@Component
public class ExternalUserServiceImpl  {

	private static Logger log = LogManager.getLogger(ExternalUserServiceImpl.class);

	@Autowired
	private KeyCloakBuilder keycloakBuilder;

	@Autowired
	private KeycloakCustomConfig keycloakCustomConfig;

	@Autowired
	private NgpSession session;

	/*
	 * @Autowired private ApplicationServiceImpl aplicationServiceImpl;
	 */
	@Autowired
	private CountryServiceImpl countryServiceImpl;

	@Autowired
	private LanguageServiceImpl languageServiceImpl;
	
	@Autowired
	private GroupServiceImpl groupServiceImpl;

	@Autowired
	DateTimeApiAuth dateTimeApiAuth;

	private static final int DEFAULT_PAGE_INDEX = 0;

	private static final String IMAGE_URL_PREFIX = "/images/";

	private static final String STATUS="status";

	private static final String USER_ID = "userId";

	private static final String USERNAME = "userName";

	private static final String NAME="name";


	public UserSummaryModel getUserSummary() {

		int active = 0, blocked = 0, expired = 0;

		List<UserRepresentation> userRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().search(null, 0, Integer.MAX_VALUE);

		for (UserRepresentation userRepresentation : userRepresentations) {
				if (userRepresentation.isEnabled()==true) {
					active++;
				}
				else if (userRepresentation.isEnabled()==false)
					blocked++;
		}

		UserSummaryModel userSummaryModel = new UserSummaryModel();
		userSummaryModel.setActive(new BigDecimal(active));
		userSummaryModel.setBlocked(new BigDecimal(blocked));
		userSummaryModel.setExpired(new BigDecimal(expired));
		userSummaryModel.setTotal(new BigDecimal(active + blocked + expired));


		return userSummaryModel;
	}

	public UserLoginStatusModel getLoginStatus(String interval,String userId) {

		UserLoginStatusModel userLoginStatusModel = new UserLoginStatusModel();
		userLoginStatusModel.setSuccessful(new BigDecimal(0));
		userLoginStatusModel.setFailed(new BigDecimal(0));

		List<String> userIds = new ArrayList<>();

		String from = getFrom(interval);


		log.debug("The from date in the login status is {}",from);


		List<String> eventsList = new ArrayList<>();
		eventsList.add(KeyCloakEventType.LOGIN.getType());
		eventsList.add(KeyCloakEventType.LOGIN_ERROR.getType());

		if (userId != null) {
			RealmResource realmResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm());
			List<UserRepresentation> userRepresentations = realmResource.users().search(userId, 0, Integer.MAX_VALUE);
			userIds = userRepresentations.stream()
					.map(userRepresentation -> userRepresentation.getId())
					.collect(Collectors.toList());
			eventsList.add(KeyCloakEventType.LOGIN.getType());
			eventsList.add(KeyCloakEventType.LOGIN_ERROR.getType());
		}else {
			userIds.add(null);
		}

		List<EventRepresentation> eventRepresentations = new ArrayList<>();
		for(String users : userIds) {
			eventRepresentations.addAll(keycloakBuilder.getInstance()
					.realm(keycloakCustomConfig.getRealm())
					.getEvents(eventsList, null, users, from, null, null, 0, Integer.MAX_VALUE));
		}

		eventRepresentations.forEach(eventRepresentation -> {

			if (eventRepresentation.getType().equals(KeyCloakEventType.LOGIN.getType()))
				userLoginStatusModel.setSuccessful(userLoginStatusModel.getSuccessful().add(new BigDecimal(1)));
			else
				userLoginStatusModel.setFailed(userLoginStatusModel.getFailed().add(new BigDecimal(1)));

		});

		return userLoginStatusModel;
	}

	/*
	 * private Stream<EventRepresentation>
	 * filterEventRepresentations(List<EventRepresentation> eventRepresentations,
	 * Long from, Long to,String userId) {
	 *
	 * Stream<EventRepresentation> stream = eventRepresentations.stream();
	 *
	 * if (from != null && to != null) return stream.filter(e -> e.getTime() >= from
	 * && e.getTime() <= to);
	 *
	 * if(userId!=null) return stream.filter(e -> e.getUserId()!=null &&
	 * e.getUserId().equals(userId));
	 *
	 * return stream; }
	 */
	public List<UserLoginInfoModel> getConnectedUsers(String interval, String filter, Integer pageSize,
			Integer pageIndex, String sort) {


		final List<UserLoginInfoModel> userLoginInfoModels = new ArrayList<>();
		List<String> eventsList = new ArrayList<>();

		Map<String, String> filterMap = UserManagementHelper.convertToMap(filter);

		List<String> userIds = new ArrayList<>();

		if (filterMap.containsKey(STATUS)) {

			if (filterMap.get(STATUS).equalsIgnoreCase("ALL")) {
				eventsList.add(KeyCloakEventType.LOGIN.getType());
				eventsList.add(KeyCloakEventType.LOGIN_ERROR.getType());
			} else if (filterMap.get(STATUS).equals(LoginStatus.SUCCESSFUL.getKey()))
				eventsList.add(KeyCloakEventType.LOGIN.getType());
			else if (filterMap.get(STATUS).equals(LoginStatus.FAILED.getKey()))
				eventsList.add(KeyCloakEventType.LOGIN_ERROR.getType());

		}

		if (filterMap.containsKey(USER_ID)) {
			RealmResource realmResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm());
			List<UserRepresentation> userRepresentations = realmResource.users().search(filterMap.get(USER_ID), 0, Integer.MAX_VALUE);
			userIds = userRepresentations.stream()
					.map(userRepresentation -> userRepresentation.getId())
					.collect(Collectors.toList());
			if(userIds.isEmpty())
				return userLoginInfoModels;
			eventsList.add(KeyCloakEventType.LOGIN.getType());
			eventsList.add(KeyCloakEventType.LOGIN_ERROR.getType());

		}else {
			userIds.add(null);
		}
		String from=getFrom(interval);

		List<EventRepresentation> EventRepresentations = new ArrayList<>();
		for(String userId : userIds) {
			EventRepresentations.addAll(keycloakBuilder.getInstance()
					.realm(keycloakCustomConfig.getRealm())
					.getEvents(eventsList, null, userId, from, null, null, 0, Integer.MAX_VALUE));
		}
		List<EventRepresentation> sortedEventRepresentations =
				getPage(EventRepresentations.stream()
				.sorted(Comparator.comparing(EventRepresentation::getTime).reversed())
				.collect(Collectors.toList()),pageIndex,pageSize);
		sortedEventRepresentations.stream().forEach(eventRepresentation -> {
			userLoginInfoModels.add(setUserLoginInfoModel(eventRepresentation));
		});

		return userLoginInfoModels;
	}

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

	private UserLoginInfoModel setUserLoginInfoModel(EventRepresentation eventRepresentation) {

		UserLoginInfoModel userLoginInfoModel = new UserLoginInfoModel();

		UserRepresentation userRepresentation=null;

		if (eventRepresentation.getUserId() != null)
			userRepresentation = findById(eventRepresentation.getUserId());

		List<String> values = getValue(userRepresentation, UserAttribute.DISPLAY_NAME);

		if (!CollectionUtils.isEmpty(values))
		{
			userLoginInfoModel.setName(getValue(userRepresentation, UserAttribute.DISPLAY_NAME).get(0));
		}
		else if (userRepresentation != null && userRepresentation.getFederatedIdentities() != null && userRepresentation.getFederatedIdentities().size() > 0){
			userLoginInfoModel.setName(userRepresentation.getFirstName() + " " + userRepresentation.getLastName());
		}
		else {
			userLoginInfoModel.setName("Invalid User");
		}
		if(userRepresentation!=null&&userRepresentation.getUsername()!=null)
			userLoginInfoModel.setUserName(userRepresentation.getUsername());
		userLoginInfoModel.setSourceAddress(eventRepresentation.getIpAddress());
		userLoginInfoModel.setDescription(KeyCloakEventType.getDescription(eventRepresentation.getType()));
		userLoginInfoModel.setDatetime(eventRepresentation.getTime());
		userLoginInfoModel.setStatus(KeyCloakEventType.getDisplay(eventRepresentation.getType()));

		return userLoginInfoModel;
	}



	/*
	 * private Predicate<EventRepresentation> getFilter(Long from, Long to, String
	 * filter) {
	 *
	 * Map<String, String> filterMap = UserManagementHelper.convertToMap(filter);
	 *
	 * List<Predicate<EventRepresentation>> predicates = new
	 * ArrayList<Predicate<EventRepresentation>>();
	 *
	 * if (from != null && to != null) predicates.add(eventRepresentation ->
	 * eventRepresentation.getTime() >= from && eventRepresentation.getTime() <=
	 * to);
	 *
	 * if (filterMap.containsKey(STATUS)) {
	 *
	 * if (filterMap.get(STATUS).equalsIgnoreCase("ALL")) predicates.add(
	 * eventRepresentation ->
	 * (eventRepresentation.getType().equals(KeyCloakEventType.LOGIN.getType()) ||
	 * eventRepresentation.getType().equals(KeyCloakEventType.LOGIN_ERROR.getType())
	 * ));
	 *
	 * else if (filterMap.get(STATUS).equals(LoginStatus.SUCCESSFUL.getKey()))
	 * predicates.add( eventRepresentation ->
	 * eventRepresentation.getType().equals(KeyCloakEventType.LOGIN.getType()));
	 * else if (filterMap.get(STATUS).equals(LoginStatus.FAILED.getKey()))
	 * predicates.add(eventRepresentation -> eventRepresentation.getType()
	 * .equals(KeyCloakEventType.LOGIN_ERROR.getType()));
	 *
	 * }
	 *
	 * if (filterMap.containsKey(USER_ID)) { String
	 * id=getKeyCloakIdFromUserName(filterMap.get(USER_ID));
	 * predicates.add(eventRepresentation -> eventRepresentation.getUserId()!=null
	 * &&eventRepresentation.getUserId().equals(id)); }
	 *
	 * return predicates.stream().reduce(Predicate::and).orElse(x -> true); }
	 */
	 

	public void changePassword(String id, ChangePasswordModel changePasswordModel) {

		CredentialRepresentation credentialRepresentation = new CredentialRepresentation();
		credentialRepresentation.setType(CredentialRepresentation.PASSWORD);
		credentialRepresentation.setValue(changePasswordModel.getNewPassword());

		UserResource userResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users()
				.get(id);

		Date date = new Date();

		UserRepresentation userRepresentation = userResource.toRepresentation();
		try {
			userResource.resetPassword(credentialRepresentation);
		} catch(BadRequestException e) {
			Response response = e.getResponse();
		    try {
		       Map error = JsonSerialization.readValue((ByteArrayInputStream) response.getEntity(), Map.class);
		        throw new ChangePasswordException(HttpStatus.BAD_REQUEST, error.get("error_description").toString());
		    } catch (IOException ex) {
		        ex.printStackTrace();
		    }
		}
		setModifyDetails(userRepresentation, date);
		userRepresentation.singleAttribute(UserAttribute.PASSWORD_CHANGED_DATE.getLabel(),
				String.valueOf(date.getTime()));
		userRepresentation.singleAttribute(UserAttribute.PASSWORD_EXPIRY_DATE.getLabel(),
				String.valueOf(CalendarUtil.addDaysToDate(date, PasswordHelper.PASSWORD_EXPIRY_DAYS).getTime()));

		userResource.update(userRepresentation);
	}

	public List<ExternalUserGridModel> getExternalUserListing(Integer pageIndex, Integer pageSize, String filter, String sort, String groupId,
			boolean isAndFilter)
	{
		List<UserRepresentation> userRepresentations=null;
		List<UserRepresentation> paginUserRepresentations=null;
		RealmResource realmResource=keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm());

		if(groupId!=null)
		{
			userRepresentations = realmResource.groups().group(groupId).members(0, Integer.MAX_VALUE);
			paginUserRepresentations = getPage(userRepresentations.stream().collect(Collectors.toList()), pageIndex,
					pageSize);
		}
		else
		{
			userRepresentations= realmResource.users().search(null, 0, Integer.MAX_VALUE);
			paginUserRepresentations = getPage(
					userRepresentations.stream().filter(getUserListFilter(filter, isAndFilter))
							.sorted(getUserrepresentationListSort(sort)).collect(Collectors.toList()),
					pageIndex, pageSize);
		}

		List<ExternalUserGridModel> externalUserGridModels = new ArrayList<>();

		// List<String> clientIds = getClientIdList();
//		userRepresentations.stream().filter(getUserListFilter(filter, isAndFilter)).filter(getDeleteFilter())
		paginUserRepresentations.stream().forEach(userRepresentation -> {

			ExternalUserGridModel externalUserGridModel = UserRepresentationToUserGridModel.convertUserRepresentationToUserGridModel(
					userRepresentation, null,
					countryServiceImpl.getCountries(), languageServiceImpl.getLanguages());
			/*
			 * List<Integer> applicationAndModuleCount =
			 * getApplicationAndModuleCount(userRepresentation.getId(), clientIds);
			 * userGridModel.setNoOfApplications(applicationAndModuleCount.get(0));
			 * userGridModel.setNoOfModules(applicationAndModuleCount.get(1));
			 */		
			
			if (userRepresentation.getFederationLink() != null) {
				externalUserGridModel.setIsLdap(true);
			} else if (userRepresentation.getAttributes().get("isAdUser") != null){
				externalUserGridModel.setIsLdap(true);
			} else {
				externalUserGridModel.setIsLdap(false);
			}
			
			externalUserGridModels.add(externalUserGridModel);
		});
		return externalUserGridModels;
	}

	public List<SearchResultItemModel> getUserListingGlobal(Integer pageIndex, Integer pageSize, String filter,
			String sort, boolean isAndFilter) {
		List<UserRepresentation> userRepresentations = null;

		RealmResource realmResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm());
		userRepresentations = realmResource.users().search(filter, 0, Integer.MAX_VALUE);

		List<SearchResultItemModel> modelList = new ArrayList<>();

		for (UserRepresentation userRepresentation : userRepresentations) {
			SearchResultItemModel model = new SearchResultItemModel();
			model.setValue(userRepresentation.getUsername());
			Map<String, List<String>> attributes = userRepresentation.getAttributes();

			if (!CollectionUtils.isEmpty(attributes.get(UserAttribute.DISPLAY_NAME.getLabel())))
				model.setName(attributes.get(UserAttribute.DISPLAY_NAME.getLabel()).get(0));
			if (!CollectionUtils.isEmpty(attributes.get(UserAttribute.NOTES.getLabel())))
				model.setDescription(attributes.get(UserAttribute.NOTES.getLabel()).get(0));

			if (userRepresentation.getFederationLink() != null) {
				model.setIsLdapUser(true);
			} else if (userRepresentation.getAttributes().get("isAdUser") != null){
				model.setIsLdapUser(true);
			} else {
				model.setIsLdapUser(false);
			}
			modelList.add(model);
		}

		return modelList;
	}

	public Boolean getIsFederated() {
		Boolean isFederationLink = false;
		List<ComponentRepresentation> compr = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).components().query();
		List<IdentityProviderRepresentation> idp = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).identityProviders().findAll();
		List<ComponentRepresentation> data = compr.stream().filter(a -> a.getProviderType().equalsIgnoreCase("org.keycloak.storage.UserStorageProvider")).collect(Collectors.toList());
		if(!data.isEmpty()) {
			isFederationLink = true;
			return isFederationLink;
		// } else if (!idp.isEmpty()){
		// 	isFederationLink = true;
		// 	return isFederationLink;
		}
		else {
			return isFederationLink;
		}
	}

		private Predicate<UserRepresentation> getDeleteFilter() {

		return userRepresentation -> userRepresentation.getAttributes() != null
				&& !CollectionUtils
						.isEmpty(userRepresentation.getAttributes().get(UserAttribute.DELETE_FLAG.getLabel()))
				&& userRepresentation.getAttributes().get(UserAttribute.DELETE_FLAG.getLabel()).get(0)
						.equalsIgnoreCase(Boolean.FALSE.toString());
	}

	public long getCountOfUsers(String filter, String groupId) {

		if (groupId != null) {
			return getMemberCountInGroups(groupId);
		}

		List<UserRepresentation> userRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().search(null, 0, Integer.MAX_VALUE);

		return userRepresentations.stream().filter(getUserListFilter(filter, true)).filter(getDeleteFilter()).count();
	}

	private long getMemberCountInGroups(String groupId) {
		
		List<UserRepresentation> userRepresentations = null;
		RealmResource realmResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm());
		userRepresentations = realmResource.groups().group(groupId).members(0, Integer.MAX_VALUE);
		return userRepresentations.stream().filter(getDeleteFilter()).count();

	}

	private Predicate<UserRepresentation> getUserListFilter(String filter, boolean isAnd) {

		Map<String, String> filterMap = UserManagementHelper.convertToMap(filter);

		List<Predicate<UserRepresentation>> predicates = new ArrayList<Predicate<UserRepresentation>>();

		if (filterMap.containsKey(STATUS))
			predicates.add(userRepresentation -> userRepresentation.getAttributes() != null
					&& !CollectionUtils.isEmpty(userRepresentation.getAttributes().get(UserAttribute.STATUS.getLabel()))
					&& userRepresentation.getAttributes().get(UserAttribute.STATUS.getLabel()).get(0)
							.equalsIgnoreCase(UserStatus.getStatusCode(filterMap.get(STATUS)).toString()));

		if (filterMap.containsKey(USERNAME)) {
			log.debug(":{} has performed search on :{} in User detail",MDC.get(USER_ID),filterMap.get(USERNAME));
			predicates.add(getUserSearchQueryFilter(filterMap.get(USERNAME)));
		}


		if(filterMap.containsKey(NAME))
		{
			predicates.add(getUserNameSearchQueryFilter(filterMap.get(NAME)));
		}
		
		if (isAnd)
			return predicates.stream().reduce(Predicate::and).orElse(userRepresentation -> true);
		else
			return predicates.stream().reduce(Predicate::or).orElse(userRepresentation -> true);
	}

	 

 
	public IdModel createExternalUser(ExternalUserModel externalUserModel) {

		Date date = new Date();
		if(externalUserModel.getEnable2FA() == null) {
			externalUserModel.setEnable2FA(false);
		}
		UserRepresentation userRepresentation = UserModelToUserRepresentation
				.convertUserModelToUserRepresentation(externalUserModel, null, date);

		setCreateDetails(userRepresentation, date);
		setModifyDetails(userRepresentation, date);
		
		if(externalUserModel.getIdentityProviderLink() != null) {
			FederatedIdentityRepresentation federatedIdentity = new FederatedIdentityRepresentation();
			federatedIdentity.setIdentityProvider(externalUserModel.getIdentityProviderLink());
			federatedIdentity.setUserId(externalUserModel.getEmail());
			federatedIdentity.setUserName(externalUserModel.getEmail());
			List<FederatedIdentityRepresentation> federatedIdentities = new ArrayList<FederatedIdentityRepresentation>();
			federatedIdentities.add(federatedIdentity);
			userRepresentation.setFederatedIdentities(federatedIdentities);
			
			userRepresentation.singleAttribute("isAdUser", "true");
		}

		Response response = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users()
				.create(userRepresentation);

		if (response.getStatus() == Response.Status.CREATED.getStatusCode()) {

			String uuid = response.getLocation().getPath().replaceAll(".*/([^/]+)$", "$1");
			assignUserToGroups(uuid, externalUserModel.getGroupIds());
			return new IdModel().id(uuid);
		} else {

			log.error("Failed to create User");
			return null;
		}

	}

  
	public UserModel getUser(String id) {

		UserRepresentation userRepresentation = findById(id);

		UserModel userModel = UserRepresentationToUserModel.convertUserRepresentationToUserModel(userRepresentation,
				getGroupsAssignedToUsers(id),dateTimeApiAuth);

		if(userRepresentation!=null) {
			UserResource userResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users().get(id);
			List<CredentialRepresentation> credentials = userResource.credentials();
			credentials.forEach(credential -> {
				if(credential.getType().equals("otp")) {
					userModel.setEnable2FA(true);
				}
			});
		}
		
		if(userRepresentation!=null && userRepresentation.getFederatedIdentities()!=null && !userRepresentation.getFederatedIdentities().isEmpty()) {
			userModel.setIdentityProviderLink(userRepresentation.getFederatedIdentities().get(0).getIdentityProvider());
		}

		return userModel;
	}
	 
 
	public List<ExternalUserGridModel> getExternalUserListingCustomized(Integer pageIndex, Integer pageSize, String filter, String sort,
			boolean isAndFilter)
	{

		List<ExternalUserRepresentation> userRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().search(null, 0, Integer.MAX_VALUE);

		List<ExternalUserGridModel> customizeduserGridModels = new ArrayList<>();

		userRepresentations.stream().filter(getUserListFilter(filter, isAndFilter))
				.forEach(userRepresentation -> {

					ExternalUserGridModel externalUserGridModel=new ExternalUserGridModel();

					externalUserserGridModel.setId(userRepresentation.getUsername());
					externalUserserGridModel.setUserName(userRepresentation.getUsername());
					externalUserserGridModel.setEmail(userRepresentation.getEmail());
					customizeduserGridModels.add(externalUserserGridModel);
				});

		return getPage(customizeduserGridModels.stream().sorted(getUserListSort(sort)).collect(Collectors.toList()),
				pageIndex,		pageSize);
	}
 
	public List<String> getExternalUserNames() {
		List<UserRepresentation> userRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().search(null, 0, Integer.MAX_VALUE);


		List<String> userNames = new ArrayList<>();

		for (UserRepresentation userRepresentation : userRepresentations) {
			userNames.add(userRepresentation.getUsername());
		}
		return userNames;
	}

	 

}
