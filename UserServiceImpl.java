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
import com.subex.ngp.usermanagement.model.UserGridModel;
import com.subex.ngp.usermanagement.model.UserLoginInfoModel;
import com.subex.ngp.usermanagement.model.UserLoginStatusModel;
import com.subex.ngp.usermanagement.model.UserModel;
import com.subex.ngp.usermanagement.model.UserSummaryModel;

@Component
public class UserServiceImpl {

	private static Logger log = LogManager.getLogger(UserServiceImpl.class);

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
	public void blockUser(String id) {

		Map<String, List<String>> attributesMap = new HashMap<String, List<String>>();
		attributesMap.put(UserAttribute.STATUS.getLabel(),
				new ArrayList<String>(Arrays.asList(UserStatus.BLOCKED.getCode().toString())));
		attributesMap.put(UserAttribute.STATUS_MODIFIED_DATE.getLabel(),
				new ArrayList<String>(Arrays.asList(String.valueOf(new Date().getTime()))));

		updateAttribute(id, attributesMap);

	}

	public void unblockUser(String id) {

		UserRepresentation userRepresentation = findById(id);

		Date date = new Date();

		Map<String, List<String>> attributesMap = new HashMap<String, List<String>>();

		attributesMap.put(UserAttribute.STATUS_MODIFIED_DATE.getLabel(),
				new ArrayList<String>(Arrays.asList(String.valueOf(date.getTime()))));

		if (!CollectionUtils.isEmpty(getValue(userRepresentation, UserAttribute.ACCOUNT_EXPIRY))) {

			String accountExpiry = getValue(userRepresentation, UserAttribute.ACCOUNT_EXPIRY).get(0);

			if (Long.valueOf(accountExpiry) <= date.getTime())
				attributesMap.put(UserAttribute.STATUS.getLabel(),
						new ArrayList<String>(Arrays.asList(UserStatus.EXPIRED.getCode().toString())));
			else
				attributesMap.put(UserAttribute.STATUS.getLabel(),
						new ArrayList<String>(Arrays.asList(UserStatus.ACTIVE.getCode().toString())));
		} else {
			attributesMap.put(UserAttribute.STATUS.getLabel(),
					new ArrayList<String>(Arrays.asList(UserStatus.ACTIVE.getCode().toString())));
		}

		updateAttribute(id, attributesMap);
	}

	public void softDeleteUser(String id) {

		Map<String, List<String>> attributesMap = new HashMap<String, List<String>>();
		attributesMap.put(UserAttribute.DELETE_FLAG.getLabel(),
				new ArrayList<String>(Arrays.asList(Boolean.TRUE.toString())));

		UserResource userResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users()
				.get(id);

		UserRepresentation userRepresentation = userResource.toRepresentation();
		userRepresentation.setEnabled(false);

		if (userRepresentation.getAttributes() == null)
			userRepresentation.setAttributes(new HashMap<String, List<String>>());

		attributesMap.forEach((key, value) -> {
			userRepresentation.getAttributes().put(key, value);
		});

		setModifyDetails(userRepresentation, new Date());

		userResource.update(userRepresentation);

	}

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

	public List<UserGridModel> getUserListing(Integer pageIndex, Integer pageSize, String filter, String sort, String groupId,
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

		List<UserGridModel> userGridModels = new ArrayList<>();

		// List<String> clientIds = getClientIdList();
//		userRepresentations.stream().filter(getUserListFilter(filter, isAndFilter)).filter(getDeleteFilter())
		paginUserRepresentations.stream().forEach(userRepresentation -> {

			UserGridModel userGridModel = UserRepresentationToUserGridModel.convertUserRepresentationToUserGridModel(
					userRepresentation, null,
					countryServiceImpl.getCountries(), languageServiceImpl.getLanguages());
			/*
			 * List<Integer> applicationAndModuleCount =
			 * getApplicationAndModuleCount(userRepresentation.getId(), clientIds);
			 * userGridModel.setNoOfApplications(applicationAndModuleCount.get(0));
			 * userGridModel.setNoOfModules(applicationAndModuleCount.get(1));
			 */		
			
			if (userRepresentation.getFederationLink() != null) {
				userGridModel.setIsLdap(true);
			} else if (userRepresentation.getAttributes().get("isAdUser") != null){
				userGridModel.setIsLdap(true);
			} else {
				userGridModel.setIsLdap(false);
			}
			
			userGridModels.add(userGridModel);
		});
		return userGridModels;
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

	private Predicate<UserRepresentation> getUserSearchQueryFilter(String str) {

		List<Predicate<UserRepresentation>> predicates = new ArrayList<Predicate<UserRepresentation>>();

		if (StringUtils.isEmpty(str))
			return predicates.stream().reduce(Predicate::or).orElse(userRepresentation -> true);

		final String value = str.toLowerCase();

		predicates.add(userRepresentation -> userRepresentation.getUsername().toLowerCase().contains(value));

		predicates.add(userRepresentation -> userRepresentation.getFirstName() != null
				&& userRepresentation.getFirstName().toLowerCase().contains(value));

		predicates.add(userRepresentation -> userRepresentation.getLastName() != null
				&& userRepresentation.getLastName().toLowerCase().contains(value));

		predicates.add(userRepresentation -> {
			return userRepresentation.getAttributes() != null
					&& !CollectionUtils.isEmpty(userRepresentation.getAttributes().get(UserAttribute.NOTES.getLabel()))
					&& userRepresentation.getAttributes().get(UserAttribute.NOTES.getLabel()).get(0).toLowerCase()
							.contains(value);
		});

		return predicates.stream().reduce(Predicate::or).orElse(userRepresentation -> true);
	}


	private Predicate<UserRepresentation> getUserNameSearchQueryFilter(String str) {

		List<Predicate<UserRepresentation>> predicates = new ArrayList<>();

		if (StringUtils.isEmpty(str))
			return predicates.stream().reduce(Predicate::or).orElse(userRepresentation -> true);

		final String value = str.toLowerCase();


		predicates.add(userRepresentation -> userRepresentation.getFirstName() != null
				&& userRepresentation.getFirstName().toLowerCase().contains(value));

		predicates.add(userRepresentation -> userRepresentation.getLastName() != null
				&& userRepresentation.getLastName().toLowerCase().contains(value));

		predicates.add(userRepresentation -> {
		    StringBuilder userName = new StringBuilder();
		    if (userRepresentation.getFirstName() != null) {
		    userName.append(userRepresentation.getFirstName());
		    }

		    if (userRepresentation.getLastName() != null) {
			userName.append(" ");
			userName.append(userRepresentation.getLastName());
		    }
		    return userName.length() != 0 && userName.toString().toLowerCase().contains(value);
		});

		return predicates.stream().reduce(Predicate::or).orElse(userRepresentation -> true);
	}




	private Comparator<UserRepresentation> getUserrepresentationListSort(String sort) {

		LinkedHashMap<String, String> sortMap = UserManagementHelper.convertToMap(sort);

		List<Comparator<UserRepresentation>> predicates = new ArrayList<>();

		sortMap.forEach((key, value) -> {

			log.debug(":{} has performed sort on column :{} in user detail", MDC.get(USER_ID), key);

			if (UserRepresentationComparator.get(key) != null)
			{
				predicates.add(UserRepresentationComparator.get(key).getComparator(value));
			}
		});

		return predicates.stream().reduce(Comparator::thenComparing)
				.orElse(Comparator.comparing(UserRepresentation::getUsername));
	}
	private Comparator<UserGridModel> getUserListSort(String sort) {

		LinkedHashMap<String, String> sortMap = UserManagementHelper.convertToMap(sort);

		List<Comparator<UserGridModel>> predicates = new ArrayList<Comparator<UserGridModel>>();

		sortMap.forEach((key, value) ->
		{
			log.debug(":{} has performed sort on column :{} in user detail",MDC.get(USER_ID),key);


			if (UserGridComparator.get(key) != null)
				predicates.add(UserGridComparator.get(key).getComparator(value));
		});

		return predicates.stream().reduce(Comparator::thenComparing)
				.orElse(Comparator.comparing(UserGridModel::getUserName));
	}

	/*
	 * private List<String> getClientIdList() { ClientsResource clients =
	 * keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).clients(
	 * ); List<ClientRepresentation> clientRepresentations = clients.findAll();
	 * clientRepresentations =
	 * aplicationServiceImpl.removeDefaultClients(clientRepresentations);
	 *
	 * return clientRepresentations.stream().map(x ->
	 * x.getId()).collect(Collectors.toList());
	 *
	 * }
	 *
	 * private List<Integer> getApplicationAndModuleCount(String userId,
	 * List<String> clientIds) { Map<String, Set<String>> clientAndRolesMap = new
	 * HashMap<String, Set<String>>(); int applicationCount = 0; RealmResource realm
	 * = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm());
	 *
	 * List<GroupRepresentation> groups = realm.users().get(userId).groups();
	 *
	 * for (GroupRepresentation groupRepresentation : groups) {
	 *
	 * GroupResource group = realm.groups().group(groupRepresentation.getId());
	 *
	 * for (String clientId : clientIds) { applicationCount =
	 * getApplicationAndModuleCountForGroup(clientAndRolesMap, applicationCount,
	 * group, clientId);
	 *
	 * } }
	 *
	 * return Arrays.asList(applicationCount, getModuleCount(clientAndRolesMap)); }
	 *
	 * private int getApplicationAndModuleCountForGroup(Map<String, Set<String>>
	 * clientAndRolesMap, int applicationCount, GroupResource group, String
	 * clientId) { RoleScopeResource clientLevelRoles =
	 * group.roles().clientLevel(clientId); Set<String> roles =
	 * clientLevelRoles.listEffective().stream().filter(x -> !x.isComposite())
	 * .map(x -> x.getName()).collect(Collectors.toSet());
	 *
	 * if (roles.size() > 0) { if (!clientAndRolesMap.containsKey(clientId)) {
	 * clientAndRolesMap.put(clientId, roles); applicationCount++;
	 *
	 * } else { Set<String> rolesSet = clientAndRolesMap.get(clientId);
	 * rolesSet.addAll(roles); } } return applicationCount; }
	 *
	 * private int getModuleCount(Map<String, Set<String>> clientAndRolesMap) {
	 * return (int) clientAndRolesMap.entrySet().stream().flatMap(x ->
	 * x.getValue().stream()).count(); }
	 */
	public IdModel createUser(UserModel userModel) {

		Date date = new Date();
		if(userModel.getEnable2FA() == null) {
			userModel.setEnable2FA(false);
		}
		UserRepresentation userRepresentation = UserModelToUserRepresentation
				.convertUserModelToUserRepresentation(userModel, null, date);

		setCreateDetails(userRepresentation, date);
		setModifyDetails(userRepresentation, date);
		
		if(userModel.getIdentityProviderLink() != null) {
			FederatedIdentityRepresentation federatedIdentity = new FederatedIdentityRepresentation();
			federatedIdentity.setIdentityProvider(userModel.getIdentityProviderLink());
			federatedIdentity.setUserId(userModel.getEmail());
			federatedIdentity.setUserName(userModel.getEmail());
			List<FederatedIdentityRepresentation> federatedIdentities = new ArrayList<FederatedIdentityRepresentation>();
			federatedIdentities.add(federatedIdentity);
			userRepresentation.setFederatedIdentities(federatedIdentities);
			
			userRepresentation.singleAttribute("isAdUser", "true");
		}

		Response response = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users()
				.create(userRepresentation);

		if (response.getStatus() == Response.Status.CREATED.getStatusCode()) {

			String uuid = response.getLocation().getPath().replaceAll(".*/([^/]+)$", "$1");
			assignUserToGroups(uuid, userModel.getGroupIds());
			return new IdModel().id(uuid);
		} else {

			log.error("Failed to create User");
			return null;
		}

	}

	public void assignUserToGroups(String uuid, List<String> groupNames) {

		if (!CollectionUtils.isEmpty(groupNames)) {
			UserResource userResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users()
					.get(uuid);

			try {
				userResource.toRepresentation();
			} catch (NotFoundException nfe) {
				return;
			}

			groupNames.forEach(name -> {
				GroupRepresentation groupRepresentation = findByGroupName(name);
				if (groupRepresentation != null)
					userResource.joinGroup(groupRepresentation.getId());
			});
		}
	}

	public GroupRepresentation findByGroupName(String name) {

		try {

			List<GroupRepresentation> keycloakGroupRepresentations = keycloakBuilder.getInstance()
					.realm(keycloakCustomConfig.getRealm()).groups().groups();
			for (GroupRepresentation groupRepresentation : keycloakGroupRepresentations) {
				if (groupRepresentation.getName().equalsIgnoreCase(name))
					return groupRepresentation;
			}
			return null;
		} catch (NotFoundException nfe) {
			return null;
		}
	}

	public boolean groupExistsById(String groupUuid) {

		if (StringUtils.isEmpty(groupUuid))
			return false;

		try {
			keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).groups().group(groupUuid)
					.toRepresentation();
			return true;
		} catch (NotFoundException nfe) {
			return false;
		}
	}

	public void updateUser(String id, UserModel userModel) {

		userModel.setId(id);
		if(userModel.getEnable2FA()==null) {
			userModel.setEnable2FA(false);
		}
		UserRepresentation keycloakUserRepresentation = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().get(userModel.getId()).toRepresentation();
		Date date = new Date();

		if(Objects.nonNull(keycloakUserRepresentation)) {
			UserResource userResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users()
					.get(keycloakUserRepresentation.getId());
			if(userModel.getEnable2FA()) {
				List<CredentialRepresentation> credentials = userResource.credentials();
				credentials.forEach(credential -> {
					if(credential.getType().equals("otp")) {
						userModel.setEnable2FA(false);
					}
				});
			}else {
				List<CredentialRepresentation> credentials = userResource.credentials();
				credentials.forEach(credential -> {
					if(credential.getType().equals("otp")) {
						userResource.removeCredential(credential.getId());
					}
				});
			}
		}

		UserRepresentation userRepresentation = UserModelToUserRepresentation
				.convertUserModelToUserRepresentation(userModel, keycloakUserRepresentation, date);
		
		if(keycloakUserRepresentation!=null && keycloakUserRepresentation.getFederatedIdentities()!=null && !keycloakUserRepresentation.getFederatedIdentities().isEmpty()) {
			userRepresentation.singleAttribute("isAdUser", "true");
		}

		setModifyDetails(userRepresentation, date);
		if(Objects.nonNull(keycloakUserRepresentation)) {
			userRepresentation.setEnabled(keycloakUserRepresentation.isEnabled());
			keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users()
				.get(keycloakUserRepresentation.getId()).update(userRepresentation);
		}

		leaveAllGroups(id);

		assignUserToGroups(id, userModel.getGroupIds());
	}

	public void leaveAllGroups(String uuid) {

		UserResource userResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users()
				.get(uuid);
		try {
			userResource.toRepresentation();
			List<GroupRepresentation> groupRepresentations = keycloakBuilder.getInstance()
					.realm(keycloakCustomConfig.getRealm()).users().get(uuid).groups();

			groupRepresentations.forEach(groupRepresentation -> {
				userResource.leaveGroup(groupRepresentation.getId());
			});

		} catch (NotFoundException nfe) {
			return;
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

	public List<GroupRepresentation> getGroupsAssignedToUsers(String userUuid) {

		UserResource userResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users()
				.get(userUuid);
		try {
			userResource.toRepresentation();
			return keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users().get(userUuid).groups();

		} catch (NotFoundException nfe) {
			return new ArrayList<GroupRepresentation>();
		}
	}

	public void uploadProfilePicture(String userName, MultipartFile multipartFile, String userImageFolder) {

		File folder = new File(userImageFolder);
		File[] listOfFiles = folder.listFiles((file) -> {
			String fileName = file.getName();
			return fileName.substring(0, fileName.lastIndexOf(".")).equals(userName) && !file.isDirectory();
		});

		Arrays.asList(listOfFiles).forEach(file -> file.delete());

		String multiPartOrgFilename = multipartFile.getOriginalFilename();
		String newFileName = userName
				+ (multiPartOrgFilename == null?"":multiPartOrgFilename.substring(multiPartOrgFilename.lastIndexOf(".")));

		try (OutputStream stream=Files.newOutputStream(Paths.get(userImageFolder, newFileName));)
		{
			stream.write(multipartFile.getBytes());

		} catch (IOException e) {
			log.error("Failed to write image to location: {} ", userImageFolder, e);
		}
	}

	public UrlModel getProfilePicture(String userName, String userImageFolder) {

		File folder = new File(userImageFolder);
		File[] listOfFiles = folder.listFiles((file) -> {
			String fileName = file.getName();
			return fileName.substring(0, fileName.lastIndexOf(".")).equals(userName) && !file.isDirectory();
		});

		if (listOfFiles.length == 1) {
			return new UrlModel().url(IMAGE_URL_PREFIX + listOfFiles[0].getName());
		}
		return null;
	}

	public UserDetailModel getUserDetail(String id) {

		UserRepresentation userRepresentation = findById(id);

		List<GroupRepresentation> groupRepresentations = getGroupsAssignedToUsers(id);

		UserDetailModel userDetailModel = UserRepresentationToUserDetailModel
				.convertUserRepresentationToUserDetailModel(userRepresentation, groupRepresentations,
						countryServiceImpl.getCountries(), languageServiceImpl.getLanguages());

		if (userRepresentation.getAttributes() != null) {
			if (!CollectionUtils.isEmpty(userRepresentation.getAttributes().get(UserAttribute.CREATED_BY.getLabel())))
				userDetailModel.setCreatedBy(getUserDisplayNameByUserName(
						userRepresentation.getAttributes().get(UserAttribute.CREATED_BY.getLabel()).get(0)));

			if (!CollectionUtils
					.isEmpty(userRepresentation.getAttributes().get(UserAttribute.REPORTING_MANAGER_ID.getLabel())))
				userDetailModel.setReportingManager(getUserDisplayNameByUserName(
						userRepresentation.getAttributes().get(UserAttribute.REPORTING_MANAGER_ID.getLabel()).get(0)));
		}

		//userDetailModel.setGroupAccesses(getUserGroupAccesses(id, groupRepresentations));

		AuditEventModel.callAuditLog("USER", "View User Details",
				"Details of  User " + userDetailModel.getDisplayName()+" Viewed  by "+MDC.get(USER_ID),
				"Details of  User viewed Succesfully");
		log.debug("Sucessfully viewed the user Detail: {}",MDC.get(USER_ID));


		return userDetailModel;
	}

	public String getUserDisplayNameById(String userUuid) {

		try {
			UserRepresentation userRepresentation = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm())
					.users().get(userUuid).toRepresentation();

			List<String> values = getValue(userRepresentation, UserAttribute.DISPLAY_NAME);

			if (!CollectionUtils.isEmpty(values))
				return values.get(0);
			else
				return null;

		} catch (NotFoundException nfe) {
			return null;
		}
	}

	public String getUserDisplayNameByUserName(String userName) {

		List<UserRepresentation> userRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().search(userName);
		if (userRepresentations.size() != 1)
			return null;

		UserRepresentation userRepresentation = userRepresentations.get(0);

		List<String> values = getValue(userRepresentation, UserAttribute.DISPLAY_NAME);

		if (!CollectionUtils.isEmpty(values))
			return values.get(0);
		else
			return null;

	}

	/*
	 * private List<UserGroupAccessModel> getUserGroupAccesses(String userUuid,
	 * List<GroupRepresentation> groupRepresentations) {
	 *
	 * List<UserGroupAccessModel> userGroupAccessModels = new
	 * ArrayList<UserGroupAccessModel>();
	 *
	 * ClientsResource clientsResource =
	 * keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm())
	 * .clients();
	 *
	 * groupRepresentations.forEach(groupRepresentation -> { UserGroupAccessModel
	 * userGroupAccessModel = new UserGroupAccessModel()
	 * .groupName(groupRepresentation.getName()).applications(new
	 * ArrayList<ApplicationModel>());
	 * removeDefaultClients(clientsResource.findAll()).forEach(clientRepresentation
	 * -> { userGroupAccessModel.getApplications()
	 * .add(getApplicationModel(clientRepresentation,
	 * keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).groups()
	 * .group(groupRepresentation.getId()).roles(), clientsResource)); });
	 *
	 * userGroupAccessModels.add(userGroupAccessModel); }); return
	 * userGroupAccessModels; }
	 *
	 * private List<ClientRepresentation>
	 * removeDefaultClients(List<ClientRepresentation> clients) {
	 *
	 * List<String> deafultClients = DefaultClientConstants.DEFAULT_CLIENT_LIST;
	 * ListIterator<ClientRepresentation> listIterator = clients.listIterator();
	 * while (listIterator.hasNext()) { ClientRepresentation client =
	 * listIterator.next(); if (deafultClients.contains(client.getName()))
	 * listIterator.remove(); } return clients; }
	 *
	 * private ApplicationModel getApplicationModel(ClientRepresentation
	 * clientRepresentation, RoleMappingResource groupRoles, ClientsResource
	 * clientsResource) {
	 *
	 * ApplicationModel applicationModel = new ApplicationModel();
	 * applicationModel.setId(clientRepresentation.getId());
	 * applicationModel.setClientId(clientRepresentation.getClientId());
	 * applicationModel.setName(clientRepresentation.getName());
	 * applicationModel.setDescription(clientRepresentation.getDescription());
	 * List<RoleRepresentation> roleRepresentationList =
	 * groupRoles.clientLevel(clientRepresentation.getId()) .listEffective();
	 * applicationModel.setModules(aplicationServiceImpl.getModules(clientsResource.
	 * get(clientRepresentation.getId()), getGroupRoles(roleRepresentationList)));
	 *
	 * return applicationModel; }
	 *
	 * private List<String> getGroupRoles(List<RoleRepresentation>
	 * roleRepresentationList) {
	 *
	 * return roleRepresentationList.stream().map(x ->
	 * x.getName()).collect(Collectors.toList()); }
	 */
	public void extendAccountExpiryDate(String id, DateModel dateModel) {

		Map<String, List<String>> attributesMap = new HashMap<String, List<String>>();
		attributesMap.put(UserAttribute.ACCOUNT_EXPIRY.getLabel(),
				new ArrayList<String>(Arrays.asList(dateModel.getDate().toString())));

		updateAttribute(id, attributesMap);
	}

	public void addUserConfiguration(ConfigurationModel configurationModel) {

		UserRepresentation userRepresentation = findByUserName(session.getUserId());

		Map<String, List<String>> attributesMap = new HashMap<String, List<String>>();
		attributesMap.put(configurationModel.getKey(), UserManagementHelper.split(configurationModel.getValue(), 255));

		updateAttribute(userRepresentation.getId(), attributesMap);
	}

	public ConfigurationModel getConfiguration(String key) {

		UserRepresentation userRepresentation = findByUserName(session.getUserId());

		ConfigurationModel configurationModel = new ConfigurationModel();
		configurationModel.setKey(key);

		if (userRepresentation.getAttributes() != null
				&& !CollectionUtils.isEmpty(userRepresentation.getAttributes().get(key)))
			configurationModel.setValue(String.join("", userRepresentation.getAttributes().get(key)));

		return configurationModel;
	}

	private List<String> getValue(UserRepresentation userRepresentation, UserAttribute userAttribute) {

		if (userRepresentation!=null && userRepresentation.getAttributes() != null) {
			List<String> values = userRepresentation.getAttributes().get(userAttribute.getLabel());

			if (values == null)
				return new ArrayList<String>();

			return userRepresentation.getAttributes().get(userAttribute.getLabel());
		}
		return new ArrayList<String>();
	}

	public boolean validate(UserModel userModel) {

		if (StringUtils.isEmpty(userModel.getUserName()) || StringUtils.isEmpty(userModel.getDisplayName()))
			return false;
		if (userModel.getReportingManagerId() != null && !existsByUserName(userModel.getReportingManagerId()))
			return false;
		if (!EmailValidator.validate(userModel.getEmail()))
			return false;
		return true;
	}

	public boolean existsByUserName(String userName) {

		List<UserRepresentation> keycloakUserRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().search(userName, true);
		if (keycloakUserRepresentations.isEmpty())
			return false;
		else
			return true;
	}

	public boolean existsByEmail(String email) {

		List<UserRepresentation> keycloakUserRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().search(email, 0, Integer.MAX_VALUE);
		return keycloakUserRepresentations.isEmpty() ? Boolean.FALSE : Boolean.TRUE;

	}


	public boolean existsById(String id) {

		try {
			keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users().get(id).toRepresentation();
			return true;
		} catch (NotFoundException nfe) {
			return false;
		}

	}

	public boolean existsByIdWithName(String id, String userName) {

		UserRepresentation userRepresentation = findByUserName(userName);

		if (userRepresentation == null)
			return true;
		if (userRepresentation.getId().equalsIgnoreCase(id))
			return true;
		return false;
	}

	public boolean existsByIdWithEmail(String id, String email) {

		UserRepresentation userRepresentation = findByEmail(email);

		if (userRepresentation == null)
			return true;
		return userRepresentation.getId().equalsIgnoreCase(id) ? Boolean.TRUE : Boolean.FALSE;

	}

	public boolean isDeleted(String id) {

		UserRepresentation userRepresentation = findById(id);

		if (userRepresentation != null) {
			List<String> values = getValue(userRepresentation, UserAttribute.DELETE_FLAG);

			if (values.size() == 1 && values.get(0).equalsIgnoreCase(Boolean.TRUE.toString()))
				return true;
		}
		return false;
	}

	public UserRepresentation findByUserName(String name) {

		try {
			List<UserRepresentation> userRepresentations = keycloakBuilder.getInstance()
					.realm(keycloakCustomConfig.getRealm()).users().search(name,true);

			if (userRepresentations.isEmpty())
				return null;

			UserRepresentation userRepresentation = userRepresentations.get(0);

			List<CredentialRepresentation> credentialRepresentations = keycloakBuilder.getInstance()
					.realm(keycloakCustomConfig.getRealm()).users().get(userRepresentation.getId()).credentials();

			userRepresentation.setCredentials(credentialRepresentations);

			return userRepresentation;
		} catch (NotFoundException nfe) {
			return null;
		}
	}

	public UserRepresentation findByEmail(String email) {

		try {
			List<UserRepresentation> userRepresentations = keycloakBuilder.getInstance()
					.realm(keycloakCustomConfig.getRealm()).users().search(email, 0, Integer.MAX_VALUE);
			if (userRepresentations.isEmpty())
				return null;

			UserRepresentation userRepresentation = userRepresentations.get(0);

			List<CredentialRepresentation> credentialRepresentations = keycloakBuilder.getInstance()
					.realm(keycloakCustomConfig.getRealm()).users().get(userRepresentation.getId()).credentials();

			userRepresentation.setCredentials(credentialRepresentations);

			return userRepresentation;
		} catch (NotFoundException nfe) {
			return null;
		}
	}

	public UserRepresentation findById(String id) {

		try {
			UserRepresentation userRepresentation = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm())
					.users().get(id).toRepresentation();

			keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users().get(id).groups();
			List<CredentialRepresentation> credentialRepresentations = keycloakBuilder.getInstance()
					.realm(keycloakCustomConfig.getRealm()).users().get(id).credentials();
			userRepresentation.setCredentials(credentialRepresentations);

			return userRepresentation;
		} catch (NotFoundException nfe) {

			log.error("ERROR: {} ", nfe.getMessage());
			return null;
		}
	}

	public void deleteUserByUserName(String userName) {
		UsersResource users = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users();

		List<UserRepresentation> userRepresentations = users.search(userName,true);

		if (userRepresentations == null || userRepresentations.size() != 1)
			return;

		UserRepresentation userRepresentation = userRepresentations.get(0);

		users.delete(userRepresentation.getId());
	}

	public void updateAttribute(String id, Map<String, List<String>> attributesMap) {

		if (attributesMap == null)
			return;

		UserResource userResource = keycloakBuilder.getInstance().realm(keycloakCustomConfig.getRealm()).users()
				.get(id);

		UserRepresentation userRepresentation = userResource.toRepresentation();

		if (userRepresentation.getAttributes() == null)
			userRepresentation.setAttributes(new HashMap<String, List<String>>());

		attributesMap.forEach((key, value) -> {
			userRepresentation.getAttributes().put(key, value);
			if(key.contentEquals(UserAttribute.STATUS.getLabel())) {
				userRepresentation.setEnabled(!value.get(0).contentEquals(UserStatus.BLOCKED.getCode().toString()));
			}
		});

		setModifyDetails(userRepresentation, new Date());

		userResource.update(userRepresentation);
	}

	private void setCreateDetails(UserRepresentation userRepresentation, Date date) {

		userRepresentation.singleAttribute(UserAttribute.CREATED_BY.getLabel(), session.getUserId());
		userRepresentation.singleAttribute(UserAttribute.CREATED_AT.getLabel(), String.valueOf(date.getTime()));
	}

	public void setModifyDetails(UserRepresentation userRepresentation, Date date) {

		userRepresentation.singleAttribute(UserAttribute.MODIFIED_BY.getLabel(), session.getUserId());
		userRepresentation.singleAttribute(UserAttribute.MODIFIED_AT.getLabel(), String.valueOf(date.getTime()));
	}

	public String getKeyCloakIdFromUserName(String userName) {

		UserRepresentation userRepresentation = findByUserName(userName);

		if (userRepresentation == null)
			return null;

		return userRepresentation.getId();
	}

	public String getUserNameFromKeyCloakId(String id) {

		UserRepresentation userRepresentation = findById(id);

		if (userRepresentation == null)
			return null;

		return userRepresentation.getUsername();
	}

	public Boolean deleteProfilePicture(String userName, String userImageFolder) {
		File folder = new File(userImageFolder);
		File[] listOfFiles = folder.listFiles((file) -> {
			String fileName = file.getName();
			return fileName.substring(0, fileName.lastIndexOf(".")).equals(userName) && !file.isDirectory();
		});

		if (listOfFiles.length == 0) {
			return Boolean.FALSE;
		}

		if (listOfFiles.length == 1) {
			return listOfFiles[0].delete();
		}
		return false;
	}

	public List<UserGridModel> getUserListingCustomized(Integer pageIndex, Integer pageSize, String filter, String sort,
			boolean isAndFilter)
	{

		List<UserRepresentation> userRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().search(null, 0, Integer.MAX_VALUE);

		List<UserGridModel> customizeduserGridModels = new ArrayList<>();

		userRepresentations.stream().filter(getUserListFilter(filter, isAndFilter))
				.forEach(userRepresentation -> {

					UserGridModel userGridModel=new UserGridModel();

					userGridModel.setId(userRepresentation.getUsername());
					userGridModel.setUserName(userRepresentation.getUsername());
					userGridModel.setEmail(userRepresentation.getEmail());
					customizeduserGridModels.add(userGridModel);
				});

		return getPage(customizeduserGridModels.stream().sorted(getUserListSort(sort)).collect(Collectors.toList()),
				pageIndex,		pageSize);
	}

	public Integer getUsersCount(String filter) {

		Integer usersCount = 0;
		Map<String, String> filterMap = new HashMap<>();
		if (filter != null) {
			filterMap = UserManagementHelper.convertToMap(filter);
		}
		
		if (filterMap.containsKey("groupName")) {
			String groupName = filterMap.get("groupName");
			if (groupName != null) {
				String groupid = groupServiceImpl.getKeyCloakIdFromName(groupName);

				usersCount += (int) getMemberCountInGroups(groupid);
				if (filterMap.size() < 2) {
					return usersCount;
				}
			}
		}
		
		List<UserRepresentation> userRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().search(null, 0, Integer.MAX_VALUE);
		usersCount += filter == null ? userRepresentations.size()
				: userRepresentations.stream().filter(getUserListFilter(filter, true)).collect(Collectors.toList())
						.size();
		return usersCount;
	}

	public List<String> getUserGroupNames(String id) {
		List<GroupRepresentation> groupRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().get(id).groups();

		List<String> groupNames = new ArrayList<>();

		for (GroupRepresentation groupRepresentation : groupRepresentations) {
			groupNames.add(groupRepresentation.getName());
		}
		return groupNames;
	}
	
	public List<String> getUserGroupIds(String id) {
		List<GroupRepresentation> groupRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().get(id).groups();

		List<String> groupIds = new ArrayList<>();

		for (GroupRepresentation groupRepresentation : groupRepresentations) {
			groupIds.add(groupRepresentation.getId());
		}
		return groupIds;
	}

	public List<String> getUserNames() {
		List<UserRepresentation> userRepresentations = keycloakBuilder.getInstance()
				.realm(keycloakCustomConfig.getRealm()).users().search(null, 0, Integer.MAX_VALUE);


		List<String> userNames = new ArrayList<>();

		for (UserRepresentation userRepresentation : userRepresentations) {
			userNames.add(userRepresentation.getUsername());
		}
		return userNames;
	}

	private String getFrom(String interval)
	{
		LocalDate to = LocalDate.now();
		String from = null;

		if (interval == null)
			return from;

		switch (Intervals.getValue(interval)) {

		case LAST_24_HOURS:
			from = to.minusDays(1L).toString();

			break;
		case LAST_7_DAYS:
			from = to.minusDays(7L).toString();
			break;
		case LAST_2_DAYS:
			from = to.minusDays(2L).toString();
			break;
		case LAST_30_DAYS:
			from = to.minusDays(30L).toString();

			break;

		default:
			from = LocalDate.now().toString();
			break;
		}

		return from;
	}

}
