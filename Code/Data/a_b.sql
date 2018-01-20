-- phpMyAdmin SQL Dump
-- version 4.7.0
-- https://www.phpmyadmin.net/
--
-- Hôte : 127.0.0.1
-- Généré le :  lun. 04 déc. 2017 à 06:57
-- Version du serveur :  10.1.24-MariaDB
-- Version de PHP :  7.1.6

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET AUTOCOMMIT = 0;
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Base de données :  `bigul`
--

-- --------------------------------------------------------

--
-- Structure de la table `a_b`
--

CREATE TABLE `a_b` (
  `idA` varchar(100) COLLATE utf8_bin NOT NULL,
  `idB` varchar(100) COLLATE utf8_bin NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

--
-- Déchargement des données de la table `a_b`
--

INSERT INTO `a_b` (`idA`, `idB`) VALUES
('1A', '1B'),
('2A', '2B'),
('3A', '3B'),
('Name', 'Name'),
('aformat', 'bformat'),
('idA', 'idB');

--
-- Index pour les tables déchargées
--

--
-- Index pour la table `a_b`
--
ALTER TABLE `a_b`
  ADD UNIQUE KEY `idA` (`idA`),
  ADD UNIQUE KEY `idB` (`idB`);
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
