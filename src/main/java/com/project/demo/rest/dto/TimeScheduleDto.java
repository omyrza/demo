package com.project.demo.rest.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Data
public class TimeScheduleDto {
	private String openTime;
	private String closedTime;
}
