package com.subex.ngp.usermanagement.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.subex.ngp.usermanagement.api.TrailApiDelegate;
import com.subex.ngp.usermanagement.model.TrailModel;
import com.subex.ngp.usermanagement.service.impl.TrailServiceImpl;

@Service
public class TrailService implements TrailApiDelegate {

	@Autowired
	private TrailServiceImpl trailServiceImpl;

	@Override
	public ResponseEntity<List<TrailModel>> getTrail() {
		return new ResponseEntity<List<TrailModel>>(trailServiceImpl.getTrail(), HttpStatus.OK);
	}

}
