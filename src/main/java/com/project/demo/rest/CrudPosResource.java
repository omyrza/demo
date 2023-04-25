package com.project.demo.rest;

import com.project.demo.config.SwaggerConfiguration;
import com.project.demo.service.WebPosService;
import io.swagger.annotations.*;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;


import java.util.Locale;

@RestController
@RequestMapping("/catalog/")
@RequiredArgsConstructor
public class CrudPosResource {
	private WebPosService webPosService;

	@ApiOperation(value = "PageableList POSes", notes = "All available poses including city, area, merchant name by current language")
	@ApiImplicitParams({
			@ApiImplicitParam(name = "page",
					value = "Page number, default: 0",
					paramType = "query",
					dataType = "int"),
			@ApiImplicitParam(name = "size",
					value = "Page size, default: 10",
					paramType = "query",
					dataType = "int"),
			@ApiImplicitParam(name = "sort",
					value = "Sorting order.  \n" +
							"Using syntax:  \nfield-name,direction  \n" +
							"Available fields: name\n" +
							"Direction: asc, desc  \n" +
							"Default: id,asc",
					paramType = "query",
					dataType = "string"),
			@ApiImplicitParam(name = BearerAuthenticationFilter.AUTHORIZATION_HEADER_NAME,
					value = "Authentication token",
					paramType = "header",
					dataType = "string",
					required = true),
			@ApiImplicitParam(name = "Accept-Language",
					value = SwaggerConfiguration.LANG_PARAMETER_DESCRIPTION,
					required = true,
					paramType = "header",
					dataType = "string")
	})
	@ApiResponses({
			@ApiResponse(code = 200, message = "OK")
	})
	@GetMapping(value = "/v3/poses/all-enabled",
			produces = MediaType.APPLICATION_JSON_VALUE)
	@PreAuthorize("hasAnyAuthority('ROLE_SUPERUSER', 'ROLE_MERCHANT_CARE_AGENT'")
	public ResponseEntity<PageableList<PointOfSalesV4Dto>> getAllEnabledPosesV2(@RequestParam(name = "merchantId", required = false) String merchantId,
	                                                                            @RequestParam(name = "name", required = false) String posName,
	                                                                            @RequestParam(name = "online", required = false) Boolean online,
	                                                                            @RequestParam(name = "operationAreaName", required = false) String operationArea,
	                                                                            @RequestParam(name = "area", required = false) String area,
	                                                                            @RequestParam(name = "city", required = false) String city,
	                                                                            @ApiIgnore Locale locale,
	                                                                            @PageableDefault(sort = "id", direction = Sort.Direction.ASC)
	                                                                            @ApiIgnore Pageable pageable) {
		PageableList<PointOfSalesV4Dto> ret = webPosService.getAllEnabledPosesV2(merchantId, posName, online, operationArea, area, city, locale, pageable);
		return new ResponseEntity<>(ret, HttpStatus.OK);
	}

}
