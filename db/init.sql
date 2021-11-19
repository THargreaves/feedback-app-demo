CREATE DATABASE IF NOT EXISTS `feedback`;
USE `feedback`

CREATE TABLE IF NOT EXISTS `proposal` (
    `proposal_id` INT unsigned NOT NULL AUTO_INCREMENT,
    `datetime` DATETIME NOT NULL,
    `title` VARCHAR(100) NOT NULL,
    `body` TEXT NOT NULL,
    `hash` CHAR(32) NOT NULL,
    `approved` BIT,
    PRIMARY KEY (`proposal_id`)
);

CREATE TABLE IF NOT EXISTS `vote` (
    `vote_id` INT unsigned NOT NULL AUTO_INCREMENT,
    `proposal_id` INT unsigned NOT NULL,
    `hash` CHAR(32) NOT NULL,
    `verified` BIT NOT NULL,
    PRIMARY KEY (`vote_id`),
    FOREIGN KEY (`proposal_id`) REFERENCES `proposal`(`proposal_id`)
);

CREATE TABLE IF NOT EXISTS `subscription` (
    `subscription_id` INT unsigned NOT NULL AUTO_INCREMENT,
    `proposal_id` INT unsigned NOT NULL,
    `username` CHAR(7) NOT NULL,
    `hash` CHAR(32),
    `verified` BIT NOT NULL,
    PRIMARY KEY (`subscription_id`),
    FOREIGN KEY (`proposal_id`) REFERENCES `proposal`(`proposal_id`)
);
