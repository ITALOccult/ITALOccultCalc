using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class Satellites
	{
		internal static SatelliteMutualEvents Mutual;

		internal static MutualLightCurve MutualLightCurve;

		internal static double SunToMoonRatio = 1.0;

		public const double Radian = 180.0 / Math.PI;

		public const double TwoPi = Math.PI * 2.0;

		public static string AppPath;

		private const double AstronomicalUnit = 149597871.0;

		private const double RadianSec = 648000.0 / Math.PI;

		private const double UnitLightTime = 0.0057756;

		private const double K0 = 7464960000.0;

		private const double kmAU = 0.0013787950648515336;

		private const double GMSaturn = 37930952.7;

		private const double GMUranus = 5794554.5;

		public static double PlanetRA = 0.0;

		public static double PlanetDec = 0.0;

		public static double PlanetDistance;

		private const double NumOfPointsAcrossShadow = 100.0;

		internal static double[] ShadowBrightness = new double[101];

		public static double U = 0.0;

		public static double UPlanet;

		public static double BPlanet;

		public static double PPlanet;

		public static double RMoon;

		public static double MoonTrueAnomaly;

		public static double dDist;

		public static double MeanDailyMotion;

		public static double SeparationPlanet;

		public static double PAPlanet;

		public static double PlanetRadiusArcSec;

		internal static bool IncludeHiddenEvents = false;

		public static double x;

		public static double y;

		public static double z;

		public static string NameOfMoon;

		private static double N;

		private static double J;

		private static double Na;

		private static double Ja;

		private static double L;

		private static double TrueAnomaly;

		private static double RadiusVector;

		private static double SemiMajorAxis;

		private static double e;

		private static double Gamma;

		private static double CurlyPi;

		private static double Theta;

		private static bool Heliocentric = false;

		public const float EarthRad = 8.79414f;

		public static float f;

		public static float PlanetRadius;

		public static float InitialScale;

		public static float PlanetDiameterKM;

		public static int NumberOfRings = 0;

		public static float[] RingRadius = new float[12];

		public static string[] RingName = new string[12];

		internal static double[] LightCurve;

		internal static double[] SunBright = new double[102];

		internal static double LightCurveStartTime_UT;

		internal static double LightCurveEndTime_UT;

		internal static double LightCurveStepInterval;

		internal static int LightCurveCount = 0;

		internal static string EventName = "Predicted light curve";

		internal static bool DisplayInMinutes = true;

		internal static bool DisplayInNTSC = false;

		internal static bool DisplayInPAL = false;

		public static void SatelliteCoordinates(double jd, int PlanetNumber, int MoonNumber, ref string MoonName, ref float MoonDiaKm, ref float Mag, ref double Sepn, ref double PA)
		{
			double dRA = 0.0;
			double dDec = 0.0;
			float NextElongation = 0f;
			SatelliteCoordinates(jd, HighAccuracy: true, PlanetcentricXYZ: false, PlanetNumber, MoonNumber, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, out var _, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag, ref NextElongation);
		}

		public static void SatelliteCoordinates(double jd, int PlanetNumber, int MoonNumber, bool ViewedFromSun, ref double Sepn, ref double PA)
		{
			double dRA = 0.0;
			double dDec = 0.0;
			float NextElongation = 0f;
			float MoonDiaKm = 0f;
			float Mag = 0f;
			string MoonName = "";
			SatelliteCoordinates(jd, HighAccuracy: true, PlanetcentricXYZ: false, PlanetNumber, MoonNumber, ViewedFromSun, ref MoonName, ref MoonDiaKm, out var _, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag, ref NextElongation);
		}

		public static void SatelliteCoordinates(double jd, int PlanetNumber, int MoonNumber, bool ViewedFromSun, ref string MoonName, ref float MoonDiaKm, ref double dRA, ref double dDec, ref float Mag)
		{
			double Sepn = 0.0;
			double PA = 0.0;
			float NextElongation = 0f;
			SatelliteCoordinates(jd, HighAccuracy: true, PlanetcentricXYZ: false, PlanetNumber, MoonNumber, ViewedFromSun, ref MoonName, ref MoonDiaKm, out var _, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag, ref NextElongation);
		}

		public static void SatelliteCoordinates(double jd, int PlanetNumber, int MoonNumber, ref string MoonName, ref float MoonDiaKm, ref double RA, ref double Dec, ref float Mag)
		{
			double Sepn = 0.0;
			double PA = 0.0;
			double x = 0.0;
			double y = 0.0;
			double z = 0.0;
			double dRA = 0.0;
			double dDec = 0.0;
			float NextElongation = 0f;
			SatelliteCoordinates(jd, HighAccuracy: true, PlanetcentricXYZ: false, PlanetNumber, MoonNumber, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, out var MoonDiaUncert, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag, ref NextElongation);
			Utilities.PlanetGeocentric(jd, PlanetNumber, 0.0, 2, physicalFlag: false, out x, out y, out z, out MoonDiaUncert, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _);
			Utilities.XYZ_to_RA_Dec(x + Satellites.x, y + Satellites.y, z + Satellites.z, out RA, out Dec, out PlanetDistance);
		}

		public static void SatelliteCoordinates(double jd, bool HighAccuracy, bool PlanetcentricXYZ, int PlanetNumber, int MoonNumber, bool ViewedFromSun, ref string MoonName, ref float MoonDiaKm, out double MoonDiaUncert, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag, ref float NextElongation)
		{
			Heliocentric = ViewedFromSun;
			MoonTrueAnomaly = 0.0;
			MoonDiaKm = SatelliteDiameter(PlanetNumber, MoonNumber, out MoonDiaUncert);
			switch (PlanetNumber)
			{
			case 4:
				switch (MoonNumber)
				{
				case 1:
					Phobos(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref dDist, ref Sepn, ref PA, ref Mag);
					MoonName = "Phobos (I)";
					break;
				case 2:
					Deimos(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Deimos (II)";
					break;
				}
				break;
			case 5:
				switch (MoonNumber)
				{
				case 1:
					Io(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Io (I)";
					break;
				case 2:
					Europa(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Europa (II)";
					break;
				case 3:
					Ganymede(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Ganymede (III)";
					break;
				case 4:
					Callisto(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Callisto (IV)";
					break;
				case 5:
					Jupiter_V_XIV_to_XVI(MoonNumber, jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Amalthea (V)";
					break;
				case 6:
					Himalia(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Himalia (VI)";
					break;
				case 7:
					Elara(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Elara (VII)";
					break;
				case 8:
					Pasiphae(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Pasiphae (VIII)";
					break;
				case 9:
					Sinope(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Sinope (IX)";
					break;
				case 10:
					Lysithea(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Lysithea (X)";
					break;
				case 11:
					Carme(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Carme (XI)";
					break;
				case 12:
					Ananke(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Ananke (XII)";
					break;
				case 13:
					Leda(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Leda (XIII)";
					break;
				case 14:
					Jupiter_V_XIV_to_XVI(MoonNumber, jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Thebe (XIV)";
					break;
				case 15:
					Jupiter_V_XIV_to_XVI(MoonNumber, jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Adrastea (XV)";
					MoonDiaUncert = 4.0;
					break;
				case 16:
					Jupiter_V_XIV_to_XVI(MoonNumber, jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Metis (XVI)";
					break;
				}
				break;
			case 6:
				switch (MoonNumber)
				{
				case 1:
					Mimas(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Mimas (I)";
					break;
				case 2:
					Enceladus(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Enceladus (II)";
					break;
				case 3:
					Tethys(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Tethys (III)";
					break;
				case 4:
					Dione(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Dione (IV)";
					break;
				case 5:
					Rhea(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Rhea (V)";
					break;
				case 6:
					Titan(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Titan (VI)";
					break;
				case 7:
					Hyperion(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Hyperion (VII)";
					break;
				case 8:
					Iapetus(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Iapetus (VIII)";
					break;
				case 9:
					Phoebe(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Phoebe (IX)";
					break;
				case 12:
					Helene(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Helene (XII)";
					break;
				case 13:
					Telesto(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Telesto (XIII)";
					break;
				case 14:
					Calypso(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Calypso (XIV)";
					break;
				}
				break;
			case 7:
				switch (MoonNumber)
				{
				case 1:
					Ariel(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Ariel (I)";
					break;
				case 2:
					Umbriel(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Umbriel (II)";
					break;
				case 3:
					Titania(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Titania (III)";
					break;
				case 4:
					Oberon(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Oberon (IV)";
					break;
				case 5:
					Miranda(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Miranda (V)";
					break;
				default:
					if (MoonNumber > 5 && MoonNumber < 16)
					{
						Inner10UranianMoons(MoonNumber, jd, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					}
					switch (MoonNumber)
					{
					case 6:
						MoonName = "Cordelia (VI)";
						break;
					case 7:
						MoonName = "Ophelia (VII)";
						break;
					case 8:
						MoonName = "Bianca (VIII)";
						break;
					case 9:
						MoonName = "Cressida (IX)";
						break;
					case 10:
						MoonName = "Desdemona (X)";
						break;
					case 11:
						MoonName = "Juliet (XI)";
						break;
					case 12:
						MoonName = "Portia (XII)";
						break;
					case 13:
						MoonName = "Rosalind (XIII)";
						break;
					case 14:
						MoonName = "Belinda (XIV)";
						break;
					case 15:
						MoonName = "Puck (XV)";
						break;
					}
					break;
				}
				break;
			case 8:
				switch (MoonNumber)
				{
				case 1:
					Triton(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Triton (I)";
					break;
				case 2:
					Nereid(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Nereid (II)";
					break;
				}
				break;
			case 9:
				switch (MoonNumber)
				{
				case 1:
					Charon(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Charon (I)";
					break;
				case 2:
					Nix(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Nix (II)";
					break;
				case 3:
					Hydra(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Hydra (III)";
					break;
				case 4:
					Kerberos(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Kerberos (IV)";
					break;
				case 5:
					Styx(jd, HighAccuracy, PlanetcentricXYZ, ref dRA, ref dDec, ref Sepn, ref PA, ref Mag);
					MoonName = "Styx (V)";
					break;
				}
				break;
			}
			NameOfMoon = MoonName;
		}

		public static string SatelliteName(int PlanetNumber, int MoonNumber)
		{
			string result = "";
			switch (PlanetNumber)
			{
			case 4:
				switch (MoonNumber)
				{
				case 1:
					result = "Phobos (I)";
					break;
				case 2:
					result = "Deimos (II)";
					break;
				}
				break;
			case 5:
				switch (MoonNumber)
				{
				case 1:
					result = "Io (I)";
					break;
				case 2:
					result = "Europa (II)";
					break;
				case 3:
					result = "Ganymede (III)";
					break;
				case 4:
					result = "Callisto (IV)";
					break;
				case 5:
					result = "Amalthea (V)";
					break;
				case 6:
					result = "Himalia (VI)";
					break;
				case 7:
					result = "Elara (VII)";
					break;
				case 8:
					result = "Pasiphae (VIII)";
					break;
				case 9:
					result = "Sinope (IX)";
					break;
				case 10:
					result = "Lysithea (X)";
					break;
				case 11:
					result = "Carme (XI)";
					break;
				case 12:
					result = "Ananke (XII)";
					break;
				case 13:
					result = "Leda (XIII)";
					break;
				case 14:
					result = "Thebe (XIV)";
					break;
				case 15:
					result = "Adrastea (XV)";
					break;
				case 16:
					result = "Metis (XVI)";
					break;
				}
				break;
			case 6:
				switch (MoonNumber)
				{
				case 1:
					result = "Mimas (I)";
					break;
				case 2:
					result = "Enceladus (II)";
					break;
				case 3:
					result = "Tethys (III)";
					break;
				case 4:
					result = "Dione (IV)";
					break;
				case 5:
					result = "Rhea (V)";
					break;
				case 6:
					result = "Titan (VI)";
					break;
				case 7:
					result = "Hyperion (VII)";
					break;
				case 8:
					result = "Iapetus (VIII)";
					break;
				case 9:
					result = "Phoebe (IX)";
					break;
				case 12:
					result = "Helene (XII)";
					break;
				case 13:
					result = "Telesto (XIII)";
					break;
				case 14:
					result = "Calypso (XIV)";
					break;
				}
				break;
			case 7:
				switch (MoonNumber)
				{
				case 1:
					result = "Ariel (I)";
					break;
				case 2:
					result = "Umbriel (II)";
					break;
				case 3:
					result = "Titania (III)";
					break;
				case 4:
					result = "Oberon (IV)";
					break;
				case 5:
					result = "Miranda (V)";
					break;
				case 6:
					result = "Cordelia (VI)";
					break;
				case 7:
					result = "Ophelia (VII)";
					break;
				case 8:
					result = "Bianca (VIII)";
					break;
				case 9:
					result = "Cressida (IX)";
					break;
				case 10:
					result = "Desdemona (X)";
					break;
				case 11:
					result = "Juliet (XI)";
					break;
				case 12:
					result = "Portia (XII)";
					break;
				case 13:
					result = "Rosalind (XIII)";
					break;
				case 14:
					result = "Belinda (XIV)";
					break;
				case 15:
					result = "Puck (XV)";
					break;
				}
				break;
			case 8:
				switch (MoonNumber)
				{
				case 1:
					result = "Triton (I)";
					break;
				case 2:
					result = "Nereid (II)";
					break;
				}
				break;
			case 9:
				switch (MoonNumber)
				{
				case 1:
					result = "Charon (I)";
					break;
				case 2:
					result = "Nix (II)";
					break;
				case 3:
					result = "Hydra (III)";
					break;
				case 4:
					result = "Kerberos (IV)";
					break;
				case 5:
					result = "Styx (V)";
					break;
				}
				break;
			}
			return result;
		}

		public static float SatelliteDiameter(int PlanetNumber, int MoonNumber)
		{
			double MoonDiaUncert;
			return SatelliteDiameter(PlanetNumber, MoonNumber, out MoonDiaUncert);
		}

		public static float SatelliteDiameter(int PlanetNumber, int MoonNumber, out double MoonDiaUncert)
		{
			float result = 1f;
			MoonDiaUncert = 0.0;
			switch (PlanetNumber)
			{
			case 4:
				switch (MoonNumber)
				{
				case 1:
					result = 22.5f;
					MoonDiaUncert = 0.0;
					break;
				case 2:
					result = 12.4f;
					MoonDiaUncert = 0.4;
					break;
				}
				break;
			case 5:
				switch (MoonNumber)
				{
				case 1:
					result = 3643.2f;
					MoonDiaUncert = 1.0;
					break;
				case 2:
					result = 3121.6f;
					MoonDiaUncert = 1.0;
					break;
				case 3:
					result = 5268.2f;
					MoonDiaUncert = 0.6;
					break;
				case 4:
					result = 4820.6f;
					MoonDiaUncert = 3.0;
					break;
				case 5:
					result = 168f;
					MoonDiaUncert = 4.0;
					break;
				case 6:
					result = 139.6f;
					MoonDiaUncert = 1.7000000476837158;
					break;
				case 7:
					result = 79.9f;
					MoonDiaUncert = 1.7000000476837158;
					break;
				case 8:
					result = 57.8f;
					MoonDiaUncert = 0.800000011920929;
					break;
				case 9:
					result = 35f;
					MoonDiaUncert = 0.6000000238418579;
					break;
				case 10:
					result = 42.2f;
					MoonDiaUncert = 0.699999988079071;
					break;
				case 11:
					result = 46.7f;
					MoonDiaUncert = 0.8999999761581421;
					break;
				case 12:
					result = 29.1f;
					MoonDiaUncert = 0.6000000238418579;
					break;
				case 13:
					result = 21.5f;
					MoonDiaUncert = 1.7000000476837158;
					break;
				case 14:
					result = 98.6f;
					MoonDiaUncert = 4.0;
					break;
				case 15:
					result = 16.4f;
					break;
				case 16:
					result = 43f;
					MoonDiaUncert = 4.0;
					break;
				}
				break;
			case 6:
				switch (MoonNumber)
				{
				case 1:
					result = 396.4f;
					MoonDiaUncert = 0.8;
					break;
				case 2:
					result = 504.2f;
					MoonDiaUncert = 0.4;
					break;
				case 3:
					result = 1062.2f;
					MoonDiaUncert = 1.2;
					break;
				case 4:
					result = 1122.8f;
					MoonDiaUncert = 0.8;
					break;
				case 5:
					result = 1527.6f;
					MoonDiaUncert = 2.0;
					break;
				case 6:
					result = 5149.5f;
					MoonDiaUncert = 0.2;
					break;
				case 7:
					result = 277.2f;
					MoonDiaUncert = 8.0;
					break;
				case 8:
					result = 1469f;
					MoonDiaUncert = 5.6;
					break;
				case 9:
					result = 213.2f;
					MoonDiaUncert = 1.6;
					break;
				case 12:
					result = 35.2f;
					MoonDiaUncert = 0.8;
					break;
				case 13:
					result = 24.8f;
					MoonDiaUncert = 0.4;
					break;
				case 14:
					result = 21.4f;
					MoonDiaUncert = 1.4;
					break;
				}
				break;
			case 7:
				switch (MoonNumber)
				{
				case 1:
					result = 1157.8f;
					MoonDiaUncert = 1.2;
					break;
				case 2:
					result = 1169.4f;
					MoonDiaUncert = 5.6;
					break;
				case 3:
					result = 1576.8f;
					MoonDiaUncert = 1.2;
					break;
				case 4:
					result = 1522.8f;
					MoonDiaUncert = 5.2;
					break;
				case 5:
					result = 471.6f;
					MoonDiaUncert = 1.4;
					break;
				case 6:
					result = 40.2f;
					MoonDiaUncert = 6.0;
					break;
				case 7:
					result = 42.8f;
					MoonDiaUncert = 8.0;
					break;
				case 8:
					result = 54f;
					MoonDiaUncert = 4.0;
					break;
				case 9:
					result = 79.6f;
					MoonDiaUncert = 4.0;
					break;
				case 10:
					result = 64f;
					MoonDiaUncert = 8.0;
					break;
				case 11:
					result = 93.6f;
					MoonDiaUncert = 8.0;
					break;
				case 12:
					result = 135.2f;
					MoonDiaUncert = 8.0;
					break;
				case 13:
					result = 72f;
					MoonDiaUncert = 12.0;
					break;
				case 14:
					result = 80.6f;
					MoonDiaUncert = 16.0;
					break;
				case 15:
					result = 162f;
					MoonDiaUncert = 4.0;
					break;
				}
				break;
			case 8:
				switch (MoonNumber)
				{
				case 1:
					result = 2706.8f;
					MoonDiaUncert = 1.8;
					break;
				case 2:
					result = 357f;
					MoonDiaUncert = 13.0;
					break;
				}
				break;
			case 9:
				switch (MoonNumber)
				{
				case 1:
					result = 1212.8f;
					MoonDiaUncert = 1.0;
					break;
				case 2:
					result = 38f;
					MoonDiaUncert = 0.0;
					break;
				case 3:
					result = 39.3f;
					MoonDiaUncert = 0.0;
					break;
				case 4:
					result = 12.6f;
					MoonDiaUncert = 0.1;
					break;
				case 5:
					result = 11f;
					MoonDiaUncert = 1.0;
					break;
				}
				break;
			}
			return result;
		}

		public static double SatelliteAlbedo(int PlanetNumber, int MoonNumber)
		{
			switch (PlanetNumber)
			{
			case 4:
				switch (MoonNumber)
				{
				case 1:
					return 0.07;
				case 2:
					return 0.08;
				}
				break;
			case 5:
				switch (MoonNumber)
				{
				case 1:
					return 0.62;
				case 2:
					return 0.68;
				case 3:
					return 0.44;
				case 4:
					return 0.19;
				case 5:
					return 0.09;
				case 6:
					return 0.03;
				case 7:
					return 0.03;
				case 8:
					return 0.1;
				case 9:
					return 0.05;
				case 10:
					return 0.06;
				case 11:
					return 0.06;
				case 12:
					return 0.06;
				case 13:
					return 0.07;
				case 14:
					return 0.05;
				case 15:
					return 0.1;
				case 16:
					return 0.06;
				}
				break;
			case 6:
				switch (MoonNumber)
				{
				case 1:
					return 0.6;
				case 2:
					return 1.0;
				case 3:
					return 0.8;
				case 4:
					return 0.7;
				case 5:
					return 0.7;
				case 6:
					return 0.22;
				case 7:
					return 0.3;
				case 8:
					return 0.2;
				case 9:
					return 0.08;
				case 12:
					return 0.7;
				case 13:
					return 1.0;
				case 14:
					return 1.0;
				}
				break;
			case 7:
				switch (MoonNumber)
				{
				case 1:
					return 0.35;
				case 2:
					return 0.19;
				case 3:
					return 0.28;
				case 4:
					return 0.25;
				case 5:
					return 0.27;
				case 6:
					return 0.07;
				case 7:
					return 0.07;
				case 8:
					return 0.07;
				case 9:
					return 0.07;
				case 10:
					return 0.07;
				case 11:
					return 0.07;
				case 12:
					return 0.07;
				case 13:
					return 0.07;
				case 14:
					return 0.07;
				case 15:
					return 0.07;
				}
				break;
			case 8:
				switch (MoonNumber)
				{
				case 1:
					return 0.76;
				case 2:
					return 0.16;
				}
				break;
			case 9:
				switch (MoonNumber)
				{
				case 1:
					return 0.36;
				case 2:
					return 0.3;
				case 3:
					return 0.3;
				}
				break;
			}
			return 0.0;
		}

		public static void TrueAnomaly_Radius(double MeanAnomaly, double e, ref double EquationOfCentre, ref double UnitaryRadiusVector)
		{
			double num = MeanAnomaly;
			double num2;
			do
			{
				num2 = (MeanAnomaly - num + e * Math.Sin(num)) / (1.0 - e * Math.Cos(num));
				num += num2;
			}
			while (Math.Abs(num2) > 1E-06);
			for (EquationOfCentre = 2.0 * Math.Atan(Math.Tan(num / 2.0) * Math.Sqrt((1.0 + e) / (1.0 - e))) - MeanAnomaly; EquationOfCentre > Math.PI; EquationOfCentre -= Math.PI * 2.0)
			{
			}
			while (EquationOfCentre < -Math.PI)
			{
				EquationOfCentre += Math.PI * 2.0;
			}
			UnitaryRadiusVector = 1.0 - e * Math.Cos(num);
			MoonTrueAnomaly = MeanAnomaly + EquationOfCentre;
		}

		internal static void e_pi_gamma_theta(double zSin, double zCos, double zetaSin, double zetaCos, ref double e, ref double CurlyPi, ref double Gamma, ref double Theta)
		{
			e = Math.Sqrt(zSin * zSin + zCos * zCos);
			CurlyPi = Math.Atan2(zSin, zCos);
			Gamma = 2.0 * Math.Asin(Math.Sqrt(zetaSin * zetaSin + zetaCos * zetaCos));
			Theta = Math.Atan2(zetaSin, zetaCos);
		}

		private static void PlanetPAandSep(double jd, int PlanetNumber, double x, double y, double z, ref double dRA, ref double dDec, ref double Sepn, ref double PA)
		{
			double GeocentricDist = 0.0;
			double X = 0.0;
			double Y = 0.0;
			double Z = 0.0;
			double num;
			double num2;
			double num3;
			double num4;
			if (!Heliocentric)
			{
				Utilities.PlanetGeocentric(jd, PlanetNumber, 1E-06, 2, out PlanetRA, out PlanetDec, out GeocentricDist);
				num = Math.Cos(PlanetRA);
				num2 = Math.Sin(PlanetRA);
				num3 = Math.Cos(PlanetDec);
				num4 = Math.Sin(PlanetDec);
			}
			else
			{
				Utilities.PlanetXYZ(jd, PlanetNumber, Heliocentric: true, 0.001, ref X, ref Y, ref Z, out var Version);
				GeocentricDist = Math.Sqrt(X * X + Y * Y + Z * Z);
				Utilities.PlanetXYZ(jd - 0.0057756 * GeocentricDist, PlanetNumber, Heliocentric: true, 0.0001, ref X, ref Y, ref Z, out Version);
				GeocentricDist = Math.Sqrt(X * X + Y * Y + Z * Z);
				num4 = Z / GeocentricDist;
				num3 = Math.Sqrt(1.0 - num4 * num4);
				PlanetRA = Math.Atan2(Y, X);
				num = X / GeocentricDist / num3;
				num2 = Y / GeocentricDist / num3;
			}
			dRA = (y * num - x * num2) / GeocentricDist;
			dDec = (z * num3 - (x * num + y * num2) * num4) / GeocentricDist;
			dDist = (0.0 - z) * num4 - (x * num + y * num2) * num3;
			double num5 = 1.0 / (1.0 - dDist / GeocentricDist);
			dRA *= num5;
			dDec *= num5;
			PA = Math.Atan2(dRA, dDec);
			Sepn = Math.Sqrt(dRA * dRA + dDec * dDec);
			if (PA < 0.0)
			{
				PA += Math.PI * 2.0;
			}
			dRA /= num3;
			SeparationPlanet = Sepn;
			PAPlanet = PA;
			PlanetDistance = GeocentricDist;
			switch (PlanetNumber)
			{
			case 5:
				PlanetRadiusArcSec = 196.94 / GeocentricDist / 2.0;
				break;
			case 6:
				PlanetRadiusArcSec = 166.66 / GeocentricDist / 2.0;
				break;
			case 7:
				PlanetRadiusArcSec = 68.56 / GeocentricDist / 2.0;
				break;
			case 8:
				PlanetRadiusArcSec = 73.12 / GeocentricDist / 2.0;
				break;
			default:
				PlanetRadiusArcSec = 10.0 / GeocentricDist / 2.0;
				break;
			}
			double num6 = Math.Cos(J) * num3 * Math.Sin(PlanetRA - N) + Math.Sin(J) * num4;
			double num7 = num3 * Math.Cos(PlanetRA - N);
			double num8 = Math.Sin(J) * num3 * Math.Sin(PlanetRA - N) - Math.Cos(J) * num4;
			double num9 = (0.0 - Math.Sin(J)) * Math.Cos(PlanetRA - N);
			double num10 = Math.Sin(J) * num4 * Math.Sin(PlanetRA - N) + Math.Cos(J) * num3;
			UPlanet = Math.PI + Math.Atan2(num6, num7);
			if (UPlanet > Math.PI * 2.0)
			{
				UPlanet -= Math.PI * 2.0;
			}
			BPlanet = Math.Atan(num8 / Math.Sqrt(num6 * num6 + num7 * num7));
			PPlanet = Math.Atan2(num9, num10);
			RMoon = SemiMajorAxis / GeocentricDist * (648000.0 / Math.PI);
		}

		private static void Phobos(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double dDist, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 4, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2441266.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			SemiMajorAxis = 6.26974E-05;
			e = 0.014979;
			Gamma = 0.01924923632024546;
			MeanDailyMotion = 1128.8445566;
			Theta = (327.9 - 0.43533 * num) % 360.0 / (180.0 / Math.PI);
			L = (232.412 + MeanDailyMotion * num + 0.001237 * num2 * num2) % 360.0 / (180.0 / Math.PI);
			CurlyPi = (278.96 + 0.435258 * num) % 360.0 / (180.0 / Math.PI);
			Na = (47.386 - 0.0014 * num2) / (180.0 / Math.PI);
			Ja = (37.271 + 0.0008 * num2) / (180.0 / Math.PI);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			MarsXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 4, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(11.8 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Deimos(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 4, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2441266.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			_ = (196.55 - 0.01801 * num) % 360.0 / (180.0 / Math.PI);
			SemiMajorAxis = 0.000156828;
			e = 0.000391;
			Gamma = 0.031243138939950493;
			MeanDailyMotion = 285.1618875;
			Theta = (240.38 - 0.018008 * num) % 360.0 / (180.0 / Math.PI);
			L = (28.963 + MeanDailyMotion * num - 0.274 * Math.Sin(Theta - 0.76498) - 2.8E-06 * num2 * num2) % 360.0 / (180.0 / Math.PI);
			CurlyPi = (111.7 + 0.017985 * num) % 360.0 / (180.0 / Math.PI);
			Na = (46.367 - 0.00138 * num2) / (180.0 / Math.PI);
			Ja = (36.623 + 0.00079 * num2) / (180.0 / Math.PI);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			MarsXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 4, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(12.89 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void MarsXYZRotation(out double x2000, out double y2000, out double z2000)
		{
			double num = Math.Sin(Gamma);
			double num2 = Math.Cos(Gamma);
			double num3 = Math.Sin(Theta);
			double num4 = Math.Cos(Theta);
			double num5 = Math.Sin(Ja);
			double num6 = Math.Cos(Ja);
			double num7 = Math.Sin(Na);
			double num8 = Math.Cos(Na);
			double num9 = RadiusVector * Math.Cos(L - Theta - Na);
			double num10 = RadiusVector * Math.Sin(L - Theta - Na);
			double num11 = 0.0;
			double num12 = num9;
			double num13 = num2 * num10 - num * num11;
			double num14 = num * num10 + num2 * num11;
			double num15 = num4 * num12 - num3 * num13;
			double num16 = num3 * num12 + num4 * num13;
			double num17 = num14;
			U = Math.Atan2(num16, num15);
			double num18 = num15;
			double num19 = num6 * num16 - num5 * num17;
			double num20 = num5 * num16 + num6 * num17;
			x2000 = num8 * num18 - num7 * num19;
			y2000 = num7 * num18 + num8 * num19;
			z2000 = num20;
			Utilities.ConvertXYZ_1950to2000(ref x2000, ref y2000, ref z2000);
			double RA = Na - Math.PI / 2.0;
			double Dec = Math.PI / 2.0 - Ja;
			Utilities.Precession(1950, 2451545.0, use2006values_Not1976: false, ref RA, ref Dec, 0.0, 0.0);
			RA += Math.PI / 2.0;
			Dec = Math.PI / 2.0 - Dec;
			double num21 = Math.Sin(Dec);
			double num22 = Math.Cos(Dec);
			double num23 = num * num3;
			double num24 = num2 * num21 + num * num22 * num4;
			double num25 = num2 * num22 - num * num21 * num4;
			double num26 = num * num22 + num2 * num21 * num4;
			double num27 = num21 * num3;
			N = Math.Atan2(num23, num24) + RA;
			J = Math.Atan2(Math.Sqrt(num23 * num23 + num24 * num24), num25);
			double num28 = Math.Atan2(num27, num26);
			for (U = L - Theta - Na + num28; U > Math.PI * 2.0; U -= Math.PI * 2.0)
			{
			}
			while (U < 0.0)
			{
				U += Math.PI * 2.0;
			}
		}

		private static void Io(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			Elements(num, out var L, out var L2, out var L3, out var L4, out var PhiL, out var Pi, out var Pi2, out var Pi3, out var Pi4, out var w, out var w2, out var w3, out var w4, out var _, out var Phi2, out var _, out var _, out var PSI, out var G, out var G2, out var PiJ, out var _);
			SemiMajorAxis = 0.002819347;
			e = 0.00413;
			Gamma = 0.000704;
			double node = w;
			MeanDailyMotion = 3.55155226247;
			double num2 = -41279.0 * Math.Cos(2.0 * L - 2.0 * L2);
			double num3 = -5596.0 * Math.Sin(Pi3 - Pi4) - 2198.0 * Math.Sin(Pi + Pi3 - 2.0 * PiJ - 2.0 * G2) + 1321.0 * Math.Sin(PhiL) - 1157.0 * Math.Sin(L - 2.0 * L2 + Pi4) - 1940.0 * Math.Sin(L - 2.0 * L2 + Pi3) - 791.0 * Math.Sin(L - 2.0 * L2 + Pi2) + 791.0 * Math.Sin(L - Pi3) + 82363.0 * Math.Sin(2.0 * L - 2.0 * L2);
			num2 = num2 + 170.0 * Math.Cos(L - L2) + 106.0 * Math.Cos(L - L3) - 96.0 * Math.Cos(L - Pi) - 2.0 * Math.Cos(L - Pi2) - 395.0 * Math.Cos(L - Pi3) - 214.0 * Math.Cos(L - Pi4) - 63.0 * Math.Cos(L + Pi3 - 2.0 * PiJ - 2.0 * G2) + 3.0 * Math.Cos(2.0 * L - 2.0 * L3) - 130.0 * Math.Cos(4.0 * L - 4.0 * L2);
			num3 = num3 - 27.0 * Math.Sin(-2.0 * PiJ + 2.0 * PSI - 2.0 * G2) - 456.0 * Math.Sin(-2.0 * PiJ + 2.0 * PSI) - 746.0 * Math.Sin(-2.0 * PiJ + w3 + PSI - 2.0 * G2) + 93.0 * Math.Sin(0.0 - w2 + PSI) - 72.0 * Math.Sin(0.0 - w3 + PSI) - 49.0 * Math.Sin(0.0 - w4 + PSI) - 324.0 * Math.Sin(G2) + 69.0 * Math.Sin(2.0 * G2) - 33.0 * Math.Sin(5.0 * G - 2.0 * G2 + Phi2) - 27.0 * Math.Sin(w3 - w4) + 146.0 * Math.Sin(w2 - w3) + 30.0 * Math.Sin(w2 - w4) - 38.0 * Math.Sin(Pi4 - PiJ) + 292.0 * Math.Sin(Pi2 - Pi3) + 156.0 * Math.Sin(Pi2 - Pi4) - 39.0 * Math.Sin(Pi - Pi3) - 26.0 * Math.Sin(Pi - Pi4) - 26.0 * Math.Sin(Pi + Pi4 - 2.0 * PiJ - 2.0 * G2) + 39.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + 4.0 * Pi4) - 32.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + Pi3 + 3.0 * Pi4) + 292.0 * Math.Sin(L - 2.0 * L2 + Pi) - 617.0 * Math.Sin(L - L2) - 270.0 * Math.Sin(L - L3) - 26.0 * Math.Sin(L - L4) + 192.0 * Math.Sin(L - Pi) + 5.0 * Math.Sin(L - Pi2) + 459.0 * Math.Sin(L - Pi4) + 147.0 * Math.Sin(L + Pi3 - 2.0 * PiJ - 2.0 * G2) + 21.0 * Math.Sin(2.0 * L - 4.0 * L2 + w2 + w3) - 200.0 * Math.Sin(2.0 * L - 4.0 * L2 + 2.0 * w2) - 35.0 * Math.Sin(2.0 * L - 2.0 * L3) - 3.0 * Math.Sin(3.0 * L - 4.0 * L2 + Pi3) + 275.0 * Math.Sin(4.0 * L - 4.0 * L2);
			num3 *= 1E-07;
			TimeCompleted(num3 / 3.55155226247, L, L2, L3, L4, PhiL, Pi, Pi2, Pi3, Pi4, w, w2, w3, w4, G, G2, out var L1c, out var L2c, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var w1c, out var w2c, out var w3c, out var w4c, out var _, out var Gc);
			double num4 = 7038.0 * Math.Sin(L1c - w1c) + 1835.0 * Math.Sin(L1c - w2c);
			num4 = num4 + 46.0 * Math.Sin(L1c - 2.0 * PiJ + PSI - 2.0 * Gc) + 329.0 * Math.Sin(L1c - w3c) + 93.0 * Math.Sin(L1c - w4c) - 311.0 * Math.Sin(L1c - PSI) + 75.0 * Math.Sin(3.0 * L1c - 4.0 * L2c + w2c);
			Satellites.L = L + num3;
			_ = Satellites.L;
			GallileanXYZRotation(ref x, ref y, ref z, PSI, node, num2, num4);
			Gallilean.GalileanSatellite(num, 1, out x, out y, out z);
			if (!XYZFlag)
			{
				PlanetPAandSep(num, 5, x, y, z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(-1.68 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Europa(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			Elements(num, out var L, out var L2, out var L3, out var L4, out var PhiL, out var Pi, out var Pi2, out var Pi3, out var Pi4, out var w, out var w2, out var w3, out var w4, out var Phi, out var Phi2, out var _, out var Phi4, out var PSI, out var G, out var G2, out var PiJ, out var _);
			SemiMajorAxis = 0.004485872;
			e = 3.0 / 320.0;
			Gamma = 0.00815;
			double node = w2;
			MeanDailyMotion = 1.7693227218;
			double num2 = -921.0 * Math.Cos(L2 - Pi2) - 3187.0 * Math.Cos(L2 - Pi3) - 1738.0 * Math.Cos(L2 - Pi4) + 93748.0 * Math.Cos(L - L2);
			double num3 = -1158.0 * Math.Sin(-2.0 * PiJ + 2.0 * PSI) + 1715.0 * Math.Sin(-2.0 * PiJ + w3 + PSI - 2.0 * G2) - 1846.0 * Math.Sin(G2) + 2397.0 * Math.Sin(Pi3 - Pi4) - 3172.0 * Math.Sin(PhiL) - 1993.0 * Math.Sin(L2 - L3) + 1844.0 * Math.Sin(L2 - Pi2) + 6394.0 * Math.Sin(L2 - Pi3) + 3451.0 * Math.Sin(L2 - Pi4) + 4159.0 * Math.Sin(L - 2.0 * L2 + Pi4) + 7571.0 * Math.Sin(L - 2.0 * L2 + Pi3) - 1491.0 * Math.Sin(L - 2.0 * L2 + Pi2) - 469.0 * Math.Sin(L - 2.0 * L2 + Pi) - 185640.0 * Math.Sin(L - L2) - 803.0 * Math.Sin(L - L3) + 915.0 * Math.Sin(2.0 * L - 2.0 * L2);
			num2 = num2 - 18.0 * Math.Cos(w2 - w3) - 27.0 * Math.Cos(2.0 * L3 - 2.0 * PiJ - 2.0 * G2) + 553.0 * Math.Cos(L2 - L3) + 45.0 * Math.Cos(L2 - L4) - 102.0 * Math.Cos(L2 - Pi) - 15.0 * Math.Cos(L2 - PiJ - G2) - 64.0 * Math.Cos(2.0 * L2 - 2.0 * L4) + 166.0 * Math.Cos(2.0 * L2 - 2.0 * w2) + 18.0 * Math.Cos(2.0 * L2 - w2 - w3) - 54.0 * Math.Cos(5.0 * L2 - 5.0 * L3) - 30.0 * Math.Cos(L - 2.0 * L2 + Pi4) - 67.0 * Math.Cos(L - 2.0 * L2 + Pi3) + 48.0 * Math.Cos(L - 2.0 * L3 + Pi4) + 107.0 * Math.Cos(L - 2.0 * L3 + Pi3) - 19.0 * Math.Cos(L - 2.0 * L3 + Pi2) + 523.0 * Math.Cos(L - L3) + 30.0 * Math.Cos(L - Pi3) - 290.0 * Math.Cos(2.0 * L - 2.0 * L2) - 91.0 * Math.Cos(2.0 * L - 2.0 * L3) + 22.0 * Math.Cos(4.0 * L - 4.0 * L2);
			num3 = num3 + 102.0 * Math.Sin(-2.0 * PiJ + 2.0 * PSI - 2.0 * G2) + 26.0 * Math.Sin(-2.0 * PiJ + w2 + PSI - 2.0 * G2) + 32.0 * Math.Sin(0.0 - w2 + PSI) + 255.0 * Math.Sin(0.0 - w3 + PSI) + 219.0 * Math.Sin(0.0 - w4 + PSI) - 263.0 * Math.Sin(2.0 * G2) + 18.0 * Math.Sin(2.0 * G - 2.0 * G2 + Phi4) + 19.0 * Math.Sin(2.0 * G - G2 + Phi) - 15.0 * Math.Sin(5.0 * G - 3.0 * G2 + Phi) - 150.0 * Math.Sin(5.0 * G - 2.0 * G2 + Phi2) + 102.0 * Math.Sin(w3 - w4) + 55.0 * Math.Sin(w2 - w3) + 72.0 * Math.Sin(Pi4 - PiJ) - 21.0 * Math.Sin(Pi3 - Pi4 + w3 - w4) - 74.0 * Math.Sin(Pi2 - Pi3) - 35.0 * Math.Sin(Pi2 - Pi4) - 31.0 * Math.Sin(Pi - Pi2) + 32.0 * Math.Sin(Pi - Pi3) + 107.0 * Math.Sin(Pi - Pi4) - 431.0 * Math.Sin(Pi + Pi3 - 2.0 * PiJ - 2.0 * G2) + 55.0 * Math.Sin(2.0 * L3 - 2.0 * PiJ - 2.0 * G2) - 110.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + 4.0 * Pi4) + 91.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + Pi3 + 3.0 * Pi4) - 25.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + 2.0 * Pi3 + 2.0 * Pi4) - 137.0 * Math.Sin(L2 - L4) + 39.0 * Math.Sin(L2 - Pi) + 30.0 * Math.Sin(L2 - PiJ - G2) - 18.0 * Math.Sin(2.0 * L2 - 3.0 * L3 + Pi4) - 39.0 * Math.Sin(2.0 * L2 - 3.0 * L3 + Pi3) + 98.0 * Math.Sin(2.0 * L2 - 2.0 * L4) - 166.0 * Math.Sin(2.0 * L2 - 2.0 * w2) - 18.0 * Math.Sin(2.0 * L2 - w2 - w3) + 72.0 * Math.Sin(5.0 * L2 - 5.0 * L3) + 30.0 * Math.Sin(L - 2.0 * L2 - Pi3 + 2.0 * PiJ + 2.0 * G2) - 111.0 * Math.Sin(L - 2.0 * L3 + Pi4) - 205.0 * Math.Sin(L - 2.0 * L3 + Pi3) + 39.0 * Math.Sin(L - 2.0 * L3 + Pi2) - 16.0 * Math.Sin(L - 2.0 * L3 + Pi);
			num3 = num3 - 19.0 * Math.Sin(L - Pi2) - 75.0 * Math.Sin(L - Pi3) - 31.0 * Math.Sin(L - Pi4) - 9.0 * Math.Sin(2.0 * L - 4.0 * L2 + w3 + PSI) + 4.0 * Math.Sin(2.0 * L - 4.0 * L2 + 2.0 * w3) - 14.0 * Math.Sin(2.0 * L - 4.0 * L2 + w2 + w3) + 150.0 * Math.Sin(2.0 * L - 4.0 * L2 + 2.0 * w2) - 11.0 * Math.Sin(2.0 * L - 4.0 * L2 + 2.0 * PiJ + 2.0 * G2) - 9.0 * Math.Sin(2.0 * L - 4.0 * L2 + Pi3 + Pi4) - 8.0 * Math.Sin(2.0 * L - 4.0 * L2 + 2.0 * Pi3) + 96.0 * Math.Sin(2.0 * L - 2.0 * L3) - 18.0 * Math.Sin(4.0 * L - 4.0 * L2);
			num3 *= 1E-07;
			TimeCompleted(num3 / 1.7693227218, L, L2, L3, L4, PhiL, Pi, Pi2, Pi3, Pi4, w, w2, w3, w4, G, G2, out var L1c, out var L2c, out var L3c, out var _, out var _, out var _, out var _, out var _, out var _, out var w1c, out var w2c, out var w3c, out var w4c, out var _, out var Gc);
			double num4 = 81575.0 * Math.Sin(L2c - w2c) + 4512.0 * Math.Sin(L2c - w3c) + 1164.0 * Math.Sin(L2c - w4c) - 3286.0 * Math.Sin(L2c - PSI);
			num4 = num4 + 17.0 * Math.Sin(L2c - 2.0 * PiJ + PSI - 3.0 * Gc) + 143.0 * Math.Sin(L2c - 2.0 * PiJ + PSI - 2.0 * Gc) - 143.0 * Math.Sin(L2c - w1c) - 19.0 * Math.Sin(L2c - PSI - Gc) + 35.0 * Math.Sin(L2c - PSI + Gc) - 28.0 * Math.Sin(L1c - 2.0 * L3c + w3c) + 273.0 * Math.Sin(L1c - 2.0 * L3c + w2c);
			Satellites.L = L2 + num3;
			_ = Satellites.L;
			GallileanXYZRotation(ref x, ref y, ref z, PSI, node, num2, num4);
			Gallilean.GalileanSatellite(num, 2, out x, out y, out z);
			if (!XYZFlag)
			{
				PlanetPAandSep(num, 5, x, y, z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(-1.41 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Ganymede(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			Elements(num, out var L, out var L2, out var L3, out var L4, out var PhiL, out var Pi, out var Pi2, out var Pi3, out var Pi4, out var w, out var w2, out var w3, out var w4, out var Phi, out var Phi2, out var Phi3, out var Phi4, out var PSI, out var G, out var G2, out var PiJ, out var _);
			SemiMajorAxis = 0.007155352;
			e = 0.001469;
			double num2 = 0.003238 * Math.Sin(w3) - 0.001688 * Math.Sin(PSI);
			double num3 = 0.003238 * Math.Cos(w3) - 0.001688 * Math.Cos(PSI);
			Gamma = Math.Sqrt(num2 * num2 + num3 * num3);
			double node = Math.Atan2(num2, num3);
			MeanDailyMotion = 0.8782079514;
			double num4 = -14691.0 * Math.Cos(L3 - Pi3) - 7894.0 * Math.Cos(L3 - Pi4) - 1758.0 * Math.Cos(2.0 * L3 - 2.0 * L4) + 6333.0 * Math.Cos(L2 - L3);
			double num5 = -1488.0 * Math.Sin(-2.0 * PiJ + 2.0 * PSI) - 2338.0 * Math.Sin(G2) + 6558.0 * Math.Sin(Pi3 - Pi4) + 523.0 * Math.Sin(Pi + Pi3 - 2.0 * PiJ - 2.0 * G2) - 943.0 * Math.Sin(L3 - L4) + 29387.0 * Math.Sin(L3 - Pi3) + 15800.0 * Math.Sin(L3 - Pi4) + 3218.0 * Math.Sin(2.0 * L3 - 2.0 * L4) - 12038.0 * Math.Sin(L2 - L3) - 662.0 * Math.Sin(L - 2.0 * L2 + Pi4) - 1246.0 * Math.Sin(L - 2.0 * L2 + Pi3) + 699.0 * Math.Sin(L - 2.0 * L2 + Pi2);
			num4 = num4 + 24.0 * Math.Cos(0.0 - w3 + PSI) - 9.0 * Math.Cos(w3 - w4) + 10.0 * Math.Cos(Pi3 - Pi4) + 294.0 * Math.Cos(L3 - L4) + 18.0 * Math.Cos(L3 - Pi2) - 23.0 * Math.Cos(L3 - PiJ - G2) - 20.0 * Math.Cos(L3 + Pi4 - 2.0 * PiJ - 2.0 * G2) - 51.0 * Math.Cos(L3 + Pi3 - 2.0 * PiJ - 2.0 * G2) + 39.0 * Math.Cos(2.0 * L3 - 3.0 * L4 + Pi4) - 11.0 * Math.Cos(2.0 * L3 - 2.0 * Pi3) - 10.0 * Math.Cos(2.0 * L3 - Pi3 - Pi4) - 27.0 * Math.Cos(2.0 * L3 - 2.0 * PiJ - 2.0 * G2) + 24.0 * Math.Cos(2.0 * L3 - 2.0 * w3) + 9.0 * Math.Cos(2.0 * L3 - w3 - w4) - 24.0 * Math.Cos(2.0 * L3 - w3 - PSI) - 16.0 * Math.Cos(3.0 * L3 - 4.0 * L4 + Pi4) - 156.0 * Math.Cos(3.0 * L3 - 3.0 * L4) - 41.0 * Math.Cos(4.0 * L3 - 4.0 * L4) - 11.0 * Math.Cos(5.0 * L3 - 5.0 * L4) + 9.0 * Math.Cos(L2 - Pi3) + 39.0 * Math.Cos(2.0 * L2 - 3.0 * L3 + Pi4) + 70.0 * Math.Cos(2.0 * L2 - 3.0 * L3 + Pi3) + 10.0 * Math.Cos(L - 2.0 * L2 + Pi4) + 20.0 * Math.Cos(L - 2.0 * L2 + Pi3) - 153.0 * Math.Cos(L - L2) + 155.0 * Math.Cos(L - L3) + 11.0 * Math.Cos(2.0 * L - 2.0 * L2);
			num5 = num5 + 10.0 * Math.Sin(0.0 - Pi3 + Pi4 - w3 + PSI) + 27.0 * Math.Sin(-2.0 * PiJ + 2.0 * PSI - 2.0 * G2) - 161.0 * Math.Sin(-2.0 * PiJ + w3 + PSI - 2.0 * G2) + 14.0 * Math.Sin(0.0 - w2 + PSI) + 411.0 * Math.Sin(0.0 - w3 + PSI) + 346.0 * Math.Sin(0.0 - w4 + PSI) - 66.0 * Math.Sin(2.0 * G2) + 10.0 * Math.Sin(G - G2 + Phi3) + 22.0 * Math.Sin(2.0 * G - 2.0 * G2 + Phi4) + 26.0 * Math.Sin(2.0 * G - G2 + Phi) + 11.0 * Math.Sin(3.0 * G - 2.0 * G2 + Phi2 + Phi3) + 9.0 * Math.Sin(3.0 * G - G2 + Phi - Phi2) - 19.0 * Math.Sin(5.0 * G - 3.0 * G2 + Phi) - 208.0 * Math.Sin(5.0 * G - 2.0 * G2 + Phi2) + 159.0 * Math.Sin(w3 - w4) + 21.0 * Math.Sin(w2 - w3) + 120.0 * Math.Sin(Pi4 - PiJ) - 57.0 * Math.Sin(Pi3 - Pi4 + w3 - w4) - 91.0 * Math.Sin(Pi2 - Pi3) - 72.0 * Math.Sin(Pi2 - Pi4) - 26.0 * Math.Sin(Pi - Pi3) - 9.0 * Math.Sin(Pi - Pi4) + 16.0 * Math.Sin(Pi + Pi4 - 2.0 * PiJ - 2.0 * G2) + 314.0 * Math.Sin(PhiL) - 10.0 * Math.Sin(L4 - Pi4) - 100.0 * Math.Sin(L3 - 2.0 * L4 + Pi4) + 83.0 * Math.Sin(L3 - 2.0 * L4 + Pi3) - 37.0 * Math.Sin(L3 - Pi2) + 7.0 * Math.Sin(L3 - Pi4 + w3 - w4) + 46.0 * Math.Sin(L3 - PiJ - G2) + 51.0 * Math.Sin(L3 + Pi4 - 2.0 * PiJ - 2.0 * G2) + 11.0 * Math.Sin(L3 + Pi3 - 2.0 * PiJ - 3.0 * G2) + 99.0 * Math.Sin(L3 + Pi3 - 2.0 * PiJ - 2.0 * G2) + 1.0 * Math.Sin(L3 + Pi - 2.0 * PiJ - 2.0 * G2) - 101.0 * Math.Sin(2.0 * L3 - 3.0 * L4 + Pi4) + 13.0 * Math.Sin(2.0 * L3 - 3.0 * L4 + Pi3) + 29.0 * Math.Sin(2.0 * L3 - 2.0 * Pi3) + 25.0 * Math.Sin(2.0 * L3 - Pi3 - Pi4) + 37.0 * Math.Sin(2.0 * L3 - 2.0 * PiJ - 2.0 * G2) - 24.0 * Math.Sin(2.0 * L3 - 2.0 * w3) - 9.0 * Math.Sin(2.0 * L3 - w3 - w4) + 24.0 * Math.Sin(2.0 * L3 - w3 - PSI) - 172.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + 4.0 * Pi4) + 141.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + Pi3 + 3.0 * Pi4) - 55.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + 2.0 * Pi3 + 2.0 * Pi4) + 27.0 * Math.Sin(3.0 * L3 - 4.0 * L4 + Pi4) + 226.0 * Math.Sin(3.0 * L3 - 3.0 * L4) + 53.0 * Math.Sin(4.0 * L3 - 4.0 * L4) + 13.0 * Math.Sin(5.0 * L3 - 5.0 * L4);
			num5 = num5 + 42.0 * Math.Sin(L2 - 3.0 * L3 + 2.0 * L4) - 24.0 * Math.Sin(L2 - Pi3) - 10.0 * Math.Sin(L2 - Pi4) - 78.0 * Math.Sin(2.0 * L2 - 3.0 * L3 + Pi4) - 133.0 * Math.Sin(2.0 * L2 - 3.0 * L3 + Pi3) + 90.0 * Math.Sin(L - 2.0 * L2 + Pi) + 190.0 * Math.Sin(L - L2) + 217.0 * Math.Sin(L - L3) + 2.0 * Math.Sin(2.0 * L - 4.0 * L2 + w3 + PSI) - 4.0 * Math.Sin(2.0 * L - 4.0 * L2 + 2.0 * w3) + 3.0 * Math.Sin(2.0 * L - 4.0 * L2 + 2.0 * w2) + 2.0 * Math.Sin(2.0 * L - 4.0 * L2 + Pi3 + Pi4) + 2.0 * Math.Sin(2.0 * L - 4.0 * L2 + 2.0 * Pi3) - 13.0 * Math.Sin(2.0 * L - 2.0 * L2);
			num5 *= 1E-07;
			TimeCompleted(num5 / 0.8782079514, L, L2, L3, L4, PhiL, Pi, Pi2, Pi3, Pi4, w, w2, w3, w4, G, G2, out var _, out var L2c, out var L3c, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var w2c, out var w3c, out var w4c, out var _, out var Gc);
			double num6 = -2793.0 * Math.Sin(L3c - w2c) + 32387.0 * Math.Sin(L3c - w3c) + 6871.0 * Math.Sin(L3c - w4c) - 16876.0 * Math.Sin(L3c - PSI);
			num6 = num6 + 37.0 * Math.Sin(L3c - 2.0 * PiJ + PSI - 3.0 * Gc) + 321.0 * Math.Sin(L3c - 2.0 * PiJ + PSI - 2.0 * Gc) - 15.0 * Math.Sin(L3c - 2.0 * PiJ + PSI - Gc) - 45.0 * Math.Sin(L3c - 2.0 * PiJ + PSI) - 45.0 * Math.Sin(L3c - PSI - Gc) + 51.0 * Math.Sin(L3c - PSI + Gc) + 10.0 * Math.Sin(2.0 * L2c - 3.0 * L3c + PSI) - 21.0 * Math.Sin(2.0 * L2c - 3.0 * L3c + w3c) + 30.0 * Math.Sin(2.0 * L2c - 3.0 * L3c + w2c);
			Satellites.L = L3 + num5;
			_ = Satellites.L;
			GallileanXYZRotation(ref x, ref y, ref z, PSI, node, num4, num6);
			Gallilean.GalileanSatellite(num, 3, out x, out y, out z);
			if (!XYZFlag)
			{
				PlanetPAandSep(num, 5, x, y, z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(-2.09 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Callisto(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			Elements(num, out var L, out var L2, out var L3, out var L4, out var PhiL, out var Pi, out var Pi2, out var Pi3, out var Pi4, out var w, out var w2, out var w3, out var w4, out var Phi, out var Phi2, out var Phi3, out var Phi4, out var PSI, out var G, out var G2, out var PiJ, out var _);
			SemiMajorAxis = 0.012585436;
			e = 0.00733;
			double num2 = 0.00443 * Math.Sin(w4) - 0.007649 * Math.Sin(PSI);
			double num3 = 0.00443 * Math.Cos(w4) - 0.007649 * Math.Cos(PSI);
			Gamma = Math.Sqrt(num2 * num2 + num3 * num3);
			double node = Math.Atan2(num2, num3);
			MeanDailyMotion = 0.37648620995;
			double num4 = 1656.0 * Math.Cos(L4 - Pi3) - 73328.0 * Math.Cos(L4 - Pi4) - 541.0 * Math.Cos(L4 + Pi4 - 2.0 * PiJ - 2.0 * G2) + 974.0 * Math.Cos(L3 - L4);
			double num5 = -4840.0 * Math.Sin(-2.0 * PiJ + 2.0 * PSI) + 2074.0 * Math.Sin(0.0 - w4 + PSI) - 5605.0 * Math.Sin(G2) - 6112.0 * Math.Sin(Pi3 - Pi4) - 3318.0 * Math.Sin(L4 - Pi3) + 146673.0 * Math.Sin(L4 - Pi4) + 1085.0 * Math.Sin(L4 + Pi4 - 2.0 * PiJ - 2.0 * G2) + 672.0 * Math.Sin(2.0 * L4 - 2.0 * Pi4);
			num4 = num4 - 19.0 * Math.Cos(0.0 - w3 + PSI) + 167.0 * Math.Cos(0.0 - w4 + PSI) + 11.0 * Math.Cos(G2) + 12.0 * Math.Cos(w3 - w4) - 13.0 * Math.Cos(Pi3 - Pi4) - 24.0 * Math.Cos(L4 - Pi4 - 2.0 * PiJ + 2.0 * PSI) - 17.0 * Math.Cos(L4 - Pi4 - G2) + 15.0 * Math.Cos(L4 - Pi4 + G2) + 30.0 * Math.Cos(L4 - Pi4 + 2.0 * PiJ - 2.0 * PSI) - 5.0 * Math.Cos(L4 - PiJ - 2.0 * G2) - 89.0 * Math.Cos(L4 - PiJ - G2) + 182.0 * Math.Cos(L4 - PiJ) - 6.0 * Math.Cos(L4 + Pi4 - 2.0 * PiJ - 4.0 * G2) - 62.0 * Math.Cos(L4 + Pi4 - 2.0 * PiJ - 3.0 * G2) + 27.0 * Math.Cos(L4 + Pi4 - 2.0 * PiJ - G2) + 6.0 * Math.Cos(L4 + Pi4 - 2.0 * PiJ) + 6.0 * Math.Cos(L4 + Pi4 - w4 - PSI) - 9.0 * Math.Cos(L4 + Pi3 - 2.0 * Pi4) + 14.0 * Math.Cos(L4 + Pi3 - 2.0 * PiJ - 2.0 * G2) + 13.0 * Math.Cos(2.0 * L4 - Pi3 - Pi4) - 269.0 * Math.Cos(2.0 * L4 - 2.0 * Pi4) - 25.0 * Math.Cos(2.0 * L4 - 2.0 * PiJ - 3.0 * G2) - 155.0 * Math.Cos(2.0 * L4 - 2.0 * PiJ - 2.0 * G2) - 12.0 * Math.Cos(2.0 * L4 - w3 - w4) + 19.0 * Math.Cos(2.0 * L4 - w3 - PSI) + 48.0 * Math.Cos(2.0 * L4 - 2.0 * w4) - 167.0 * Math.Cos(2.0 * L4 - w4 - PSI) + 142.0 * Math.Cos(2.0 * L4 - 2.0 * PSI) - 22.0 * Math.Cos(L3 - 2.0 * L4 + Pi4) + 20.0 * Math.Cos(L3 - 2.0 * L4 + Pi3) + 24.0 * Math.Cos(2.0 * L3 - 3.0 * L4 + Pi4) + 177.0 * Math.Cos(2.0 * L3 - 2.0 * L4) + 4.0 * Math.Cos(3.0 * L3 - 4.0 * L4 + Pi4) + 42.0 * Math.Cos(3.0 * L3 - 3.0 * L4) + 14.0 * Math.Cos(4.0 * L3 - 4.0 * L4) + 5.0 * Math.Cos(5.0 * L3 - 5.0 * L4) - 8.0 * Math.Cos(L2 - 3.0 * L3 + 2.0 * L4) + 92.0 * Math.Cos(L2 - L4) + 104.0 * Math.Cos(L - L4);
			num5 = num5 + 8.0 * Math.Sin(0.0 - Pi3 - Pi4 + 2.0 * PSI) - 9.0 * Math.Sin(0.0 - Pi3 - Pi4 + w4 + PSI) + 27.0 * Math.Sin(0.0 - Pi3 + Pi4 - w4 + PSI) - 407.0 * Math.Sin(-2.0 * Pi4 + 2.0 * PSI) + 309.0 * Math.Sin(-2.0 * Pi4 + w4 + PSI) - 19.0 * Math.Sin(-2.0 * Pi4 + w3 + PSI) + 8.0 * Math.Sin(0.0 - Pi4 - PiJ + 2.0 * PSI) - 5.0 * Math.Sin(0.0 - Pi4 - PiJ + w4 + PSI) + 63.0 * Math.Sin(0.0 - Pi4 + PiJ - w4 + PSI) + 8.0 * Math.Sin(-2.0 * PiJ + 2.0 * PSI - 3.0 * G2) + 73.0 * Math.Sin(-2.0 * PiJ + 2.0 * PSI - 2.0 * G2) + 16.0 * Math.Sin(-2.0 * PiJ + w4 + PSI - 2.0 * G2) - 97.0 * Math.Sin(0.0 - w3 + PSI) + 152.0 * Math.Sin(-2.0 * w4 + 2.0 * PSI) - 204.0 * Math.Sin(2.0 * G2) - 10.0 * Math.Sin(3.0 * G2) + 24.0 * Math.Sin(G - G2 + Phi3) + 11.0 * Math.Sin(G + Phi - 2.0 * Phi2) + 52.0 * Math.Sin(2.0 * G - 2.0 * G2 + Phi4) + 61.0 * Math.Sin(2.0 * G - G2 + Phi) + 25.0 * Math.Sin(3.0 * G - 2.0 * G2 + Phi2 + Phi3) + 21.0 * Math.Sin(3.0 * G - G2 + Phi - Phi2) - 45.0 * Math.Sin(5.0 * G - 3.0 * G2 + Phi) - 495.0 * Math.Sin(5.0 * G - 2.0 * G2 + Phi2) - 43.0 * Math.Sin(w3 - w4) + 5.0 * Math.Sin(Pi4 - PiJ - G2) + 234.0 * Math.Sin(Pi4 - PiJ) + 11.0 * Math.Sin(2.0 * Pi4 - 2.0 * PiJ - 2.0 * G2) - 10.0 * Math.Sin(2.0 * Pi4 - w3 - w4) + 68.0 * Math.Sin(2.0 * Pi4 - 2.0 * w4) - 13.0 * Math.Sin(Pi3 - Pi4 - w4 + PSI) - 42.0 * Math.Sin(Pi3 - Pi4 + w3 - w4) + 48.0 * Math.Sin(L4 - Pi4 - 2.0 * PiJ + 2.0 * PSI) + 10.0 * Math.Sin(L4 - Pi4 - w4 + PSI) + 33.0 * Math.Sin(L4 - Pi4 - G2) - 31.0 * Math.Sin(L4 - Pi4 + G2) - 6.0 * Math.Sin(L4 - Pi4 + w4 - PSI) - 61.0 * Math.Sin(L4 - Pi4 + 2.0 * PiJ - 2.0 * PSI) + 10.0 * Math.Sin(L4 - PiJ - 2.0 * G2) + 178.0 * Math.Sin(L4 - PiJ - G2) - 363.0 * Math.Sin(L4 - PiJ) + 5.0 * Math.Sin(L4 + Pi4 - 2.0 * PiJ - 5.0 * G + 2.0 * G2 - Phi) + 12.0 * Math.Sin(L4 + Pi4 - 2.0 * PiJ - 4.0 * G2) + 124.0 * Math.Sin(L4 + Pi4 - 2.0 * PiJ - 3.0 * G2) - 55.0 * Math.Sin(L4 + Pi4 - 2.0 * PiJ - G2) - 12.0 * Math.Sin(L4 + Pi4 - 2.0 * PiJ) - 13.0 * Math.Sin(L4 + Pi4 - w4 - PSI) + 6.0 * Math.Sin(L4 + Pi4 - 2.0 * PSI) + 17.0 * Math.Sin(L4 + Pi3 - 2.0 * Pi4) - 28.0 * Math.Sin(L4 + Pi3 - 2.0 * PiJ - 2.0 * G2);
			num5 = num5 - 33.0 * Math.Sin(2.0 * L4 - Pi3 - Pi4) + 36.0 * Math.Sin(2.0 * L4 - 2.0 * PiJ - 3.0 * G2) + 218.0 * Math.Sin(2.0 * L4 - 2.0 * PiJ - 2.0 * G2) - 5.0 * Math.Sin(2.0 * L4 - 2.0 * PiJ - G2) + 12.0 * Math.Sin(2.0 * L4 - w3 - w4) - 19.0 * Math.Sin(2.0 * L4 - w3 - PSI) - 48.0 * Math.Sin(2.0 * L4 - 2.0 * w4) + 167.0 * Math.Sin(2.0 * L4 - w4 - PSI) - 142.0 * Math.Sin(2.0 * L4 - 2.0 * PSI) + 148.0 * Math.Sin(L3 - 2.0 * L4 + Pi4) - 96.0 * Math.Sin(L3 - 2.0 * L4 + Pi3) - 390.0 * Math.Sin(L3 - L4) + 9.0 * Math.Sin(2.0 * L3 - 4.0 * L4 + 2.0 * Pi4) - 37.0 * Math.Sin(2.0 * L3 - 3.0 * L4 + Pi4) + 6.0 * Math.Sin(2.0 * L3 - 3.0 * L4 + Pi3) - 195.0 * Math.Sin(2.0 * L3 - 2.0 * L4) + 6.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + 2.0 * Pi4 + w4 + PSI) + 185.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + 4.0 * Pi4) - 151.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + Pi3 + 3.0 * Pi4) + 53.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + 2.0 * Pi3 + 2.0 * Pi4) - 10.0 * Math.Sin(3.0 * L3 - 7.0 * L4 + 3.0 * Pi3 + Pi4) + 6.0 * Math.Sin(3.0 * L3 - 6.0 * L4 + 3.0 * Pi4) - 8.0 * Math.Sin(3.0 * L3 - 4.0 * L4 + Pi4) - 41.0 * Math.Sin(3.0 * L3 - 3.0 * L4) - 13.0 * Math.Sin(4.0 * L3 - 4.0 * L4) - 44.0 * Math.Sin(L2 - 3.0 * L3 + 2.0 * L4) + 89.0 * Math.Sin(L2 - L4) + 105.0 * Math.Sin(L - L4);
			num5 *= 1E-07;
			TimeCompleted(num5 / 0.37648620995, L, L2, L3, L4, PhiL, Pi, Pi2, Pi3, Pi4, w, w2, w3, w4, G, G2, out var _, out var _, out var L3c, out var L4c, out var _, out var _, out var _, out var _, out var _, out var w1c, out var w2c, out var w3c, out var w4c, out var G1c, out var Gc);
			double num6 = 773.0 * Math.Sin(L4c - 2.0 * PiJ + PSI - 2.0 * Gc) - 5075.0 * Math.Sin(L4c - w3c) + 44300.0 * Math.Sin(L4c - w4c) - 76493.0 * Math.Sin(L4c - PSI);
			num6 = num6 + 8.0 * Math.Sin(L4c - 2.0 * PiJ - w4c + 2.0 * PSI) + 8.0 * Math.Sin(L4c - 2.0 * PiJ + PSI - 4.0 * Gc) + 88.0 * Math.Sin(L4c - 2.0 * PiJ + PSI - 3.0 * Gc) - 38.0 * Math.Sin(L4c - 2.0 * PiJ + PSI - Gc) + 5.0 * Math.Sin(L4c - 2.0 * PiJ + PSI) + 9.0 * Math.Sin(L4c - w1c) - 1.0 * Math.Sin(L4c - w2c) - 7.0 * Math.Sin(L4c - w4c - Gc) + 7.0 * Math.Sin(L4c - w4c + Gc) - 102.0 * Math.Sin(L4c - PSI - Gc) + 104.0 * Math.Sin(L4c - PSI + Gc) - 10.0 * Math.Sin(L4c - PSI + 5.0 * G1c - 2.0 * Gc + Phi2) - 11.0 * Math.Sin(L3c - 2.0 * L4c + PSI) + 7.0 * Math.Sin(L3c - 2.0 * L4c + w4c);
			Satellites.L = L4 + num5;
			_ = Satellites.L;
			GallileanXYZRotation(ref x, ref y, ref z, PSI, node, num4, num6);
			Gallilean.GalileanSatellite(num, 4, out x, out y, out z);
			if (!XYZFlag)
			{
				PlanetPAandSep(num, 5, x, y, z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(-1.05 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Elements(double jd, out double L1, out double L2, out double L3, out double L4, out double PhiL, out double Pi1, out double Pi2, out double Pi3, out double Pi4, out double w1, out double w2, out double w3, out double w4, out double Phi1, out double Phi2, out double Phi3, out double Phi4, out double PSI, out double G1, out double G, out double PiJ, out double NodeJ)
		{
			double num = jd - 2443000.5;
			L1 = (106.07859 + 203.488955363064 * num) % 360.0 / (180.0 / Math.PI);
			L2 = (175.733787 + 101.374724556625 * num) % 360.0 / (180.0 / Math.PI);
			L3 = (120.5613855 + 50.3176091534046 * num) % 360.0 / (180.0 / Math.PI);
			L4 = (84.455823 + 21.5710708751796 * num) % 360.0 / (180.0 / Math.PI);
			PhiL = (184.415351 + 0.17356902 * num) % 360.0 / (180.0 / Math.PI);
			Pi1 = (82.380231 + 0.16102275 * num) % 360.0 / (180.0 / Math.PI);
			Pi2 = (128.960393 + 0.04645644 * num) % 360.0 / (180.0 / Math.PI);
			Pi3 = (187.550171 + 0.00712408 * num) % 360.0 / (180.0 / Math.PI);
			Pi4 = (335.309254 + 0.00183939 * num) % 360.0 / (180.0 / Math.PI);
			w1 = (308.365749 - 0.1328061 * num) % 360.0 / (180.0 / Math.PI);
			w2 = (100.438938 - 0.03261535 * num) % 360.0 / (180.0 / Math.PI);
			w3 = (118.908928 - 0.00717678 * num) % 360.0 / (180.0 / Math.PI);
			w4 = (322.746564 - 0.00176018 * num) % 360.0 / (180.0 / Math.PI);
			Phi1 = 3.288955526016935;
			Phi2 = 0.9100948994156515;
			Phi3 = 4.493542145477121;
			Phi4 = 2.6064155684783383;
			PSI = (316.500101 - 2.48E-06 * num) / (180.0 / Math.PI);
			G1 = (31.9785280244 + 0.033459733896 * num) % 360.0 / (180.0 / Math.PI);
			G = (30.2380210168 + 0.0830925617896945 * num) % 360.0 / (180.0 / Math.PI);
			PiJ = 0.23510274429418157;
			NodeJ = 1.7452863168947306;
		}

		private static void TimeCompleted(double TC, double L1, double L2, double L3, double L4, double PhiL, double Pi1, double Pi2, double Pi3, double Pi4, double w1, double w2, double w3, double w4, double G1, double G, out double L1c, out double L2c, out double L3c, out double L4c, out double PhiLc, out double Pi1c, out double Pi2c, out double Pi3c, out double Pi4c, out double w1c, out double w2c, out double w3c, out double w4c, out double G1c, out double Gc)
		{
			L1c = L1 + 203.488955363 * TC / (180.0 / Math.PI);
			L2c = L2 + 101.37472455 * TC / (180.0 / Math.PI);
			L3c = L3 + 50.3176091 * TC / (180.0 / Math.PI);
			L4c = L4 + 21.5710708 * TC / (180.0 / Math.PI);
			PhiLc = PhiL + 0.173569 * TC / (180.0 / Math.PI);
			Pi1c = Pi1 + 0.16102275 * TC / (180.0 / Math.PI);
			Pi2c = Pi2 + 0.04645644 * TC / (180.0 / Math.PI);
			Pi3c = Pi3 + 0.00712408 * TC / (180.0 / Math.PI);
			Pi4c = Pi4 + 0.00183939 * TC / (180.0 / Math.PI);
			w1c = w1 - 0.1328061 * TC / (180.0 / Math.PI);
			w2c = w2 - 0.03261535 * TC / (180.0 / Math.PI);
			w3c = w3 - 0.00717678 * TC / (180.0 / Math.PI);
			w4c = w4 - 0.00176018 * TC / (180.0 / Math.PI);
			G1c = G1 + 0.033459733896 * TC / (180.0 / Math.PI);
			Gc = G + 0.0830925618 * TC / (180.0 / Math.PI);
		}

		private static void GallileanXYZRotation(ref double x2000, ref double y2000, ref double z2000, double PSI, double Node, double Xi, double Zeta)
		{
			double num = Math.Sin(PSI - 1.74528631689473);
			double num2 = Math.Cos(PSI - 1.74528631689473);
			double num3 = SemiMajorAxis * (1.0 + Xi * 1E-07);
			double num4 = num3 * Math.Cos(L - PSI);
			double num5 = num3 * Math.Sin(L - PSI);
			double num6 = num3 * Zeta * 1E-07;
			double num7 = num4;
			double num8 = 0.99852779 * num5 - 0.054242459 * num6;
			double num9 = 0.054242459 * num5 + 0.99852779 * num6;
			double num10 = num2 * num7 - num * num8;
			double num11 = num * num7 + num2 * num8;
			double num12 = num9;
			double num13 = num10;
			double num14 = 0.999739918 * num11 - 0.0228056363 * num12;
			double num15 = 0.0228056363 * num11 + 0.999739918 * num12;
			double num16 = -0.173605895 * num13 - 0.984815208 * num14;
			double num17 = 0.984815208 * num13 + -0.173605895 * num14;
			double num18 = num15;
			double num19 = num16;
			double num20 = 0.917436945 * num17 - 0.397881203 * num18;
			double num21 = 0.397881203 * num17 + 0.917436945 * num18;
			x2000 = num19;
			y2000 = num20;
			z2000 = num21;
			Utilities.ConvertXYZ_1950to2000(ref x2000, ref y2000, ref z2000);
			double num22 = 0.054242459 * num;
			double num23 = 0.022772061614182777 + 0.05422835151277836 * num2;
			double num24 = Math.Atan2(num22, num23);
			double num25 = Math.Sqrt(num22 * num22 + num23 * num23);
			double num26 = Math.Sqrt(1.0 - num25 * num25);
			num22 = 0.0228056363 * num;
			num23 = 0.05422835151277836 + 0.022772061614182777 * num2;
			double num27 = Math.Atan2(num22, num23);
			num22 = num25 * Math.Sin(1.74528631689473 + num24);
			num23 = num26 * 0.39777715575 + num25 * 0.91748206214 * Math.Cos(1.74528631689473 + num24 + 0.01218653);
			double num28 = num26 * 0.91748206214 - num25 * 0.39777715575 * Math.Cos(1.74528631689473 + num24 + 0.01218653);
			N = Math.Atan2(num22, num23);
			J = Math.Atan(Math.Sqrt(num22 * num22 + num23 * num23) / num28);
			num22 = Math.Sin(1.74528631689473 + num24) * 0.39777715575;
			num23 = num25 * 0.91748206214 + num26 * 0.39777715575 * Math.Cos(1.74528631689473 + num24 + 0.01218653);
			double num29 = Math.Atan2(num22, num23);
			num22 = Math.Sin(Gamma) * Math.Sin(Node - PSI + num29 + num27);
			num23 = Math.Cos(Gamma) * Math.Sin(J) + Math.Sin(Gamma) * Math.Cos(J) * Math.Cos(Node - PSI + num29 + num27);
			num28 = Math.Cos(Gamma) * Math.Cos(J) - Math.Sin(Gamma) * Math.Sin(J) * Math.Cos(Node - PSI + num29 + num27);
			double num30 = Math.Sin(J) * Math.Sin(Node - PSI + num29 + num27);
			double num31 = Math.Sin(Gamma) * Math.Cos(J) + Math.Cos(Gamma) * Math.Sin(J) * Math.Cos(Node - PSI + num29 + num27);
			N += Math.Atan2(num22, num23);
			J = Math.Atan(Math.Sqrt(num22 * num22 + num23 * num23) / num28);
			for (U = L - Node + Math.Atan2(num30, num31); U > Math.PI * 2.0; U -= Math.PI * 2.0)
			{
			}
			while (U < 0.0)
			{
				U += Math.PI * 2.0;
			}
		}

		private static void Himalia(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			double num3 = num2 / 100.0;
			MeanDailyMotion = 0.02507654668090349;
			SemiMajorAxis = 0.076;
			L = (1.8829782239 + 9.1592086752 * num2 + 0.025 + num3 * (-0.091 + num3 * (-0.206 - 0.026 * num3)) + 0.008 * Math.Sin(-0.718 + 17.2588 * num2)) * (180.0 / Math.PI) % 360.0 / (180.0 / Math.PI);
			double num4 = 0.117 + num3 * (-0.186 + num3 * (-0.26 + 0.064 * num3)) + 0.034 * Math.Sin(3.039 + 0.0633 * num2) + 0.015 * Math.Sin(2.276 + 1.0377 * num2);
			double num5 = 0.105 + num3 * (0.3 + num3 * (-0.255 - 0.294 * num3)) + 0.026 * Math.Sin(-1.964 + 0.0633 * num2) + 0.016 * Math.Sin(0.567 + 1.0346 * num2);
			double num6 = 0.284 + num3 * (0.48 + num3 * (-0.172 - 0.327 * num3));
			double num7 = 0.242 + num3 * (-0.17 + num3 * (-0.504 - 0.061 * num3));
			N = Math.Atan2(num7, num6);
			J = 2.0 * Math.Asin(Math.Sqrt(num7 * num7 + num6 * num6));
			e = Math.Sqrt(num5 * num5 + num4 * num4);
			CurlyPi = Math.Atan2(num5, num4);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre - N;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			JupiterVI_XIII_XYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && JVI_XIII_AccurateXYZ(num + 2451545.0, 6, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jdInit, 5, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(8.14 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Elara(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			double num3 = num2 / 100.0;
			MeanDailyMotion = 0.02420359568295688;
			SemiMajorAxis = 0.078;
			L = (4.0754283679 + 8.8403633232 * num2 + 0.024 + num3 * (-0.032 + num3 * (-0.233 - 0.164 * num3)) + 0.008 * Math.Sin(1.55 + 1.0074 * num2) + 0.012 * Math.Sin(1.035 + 7.8051 * num2) + 0.008 * Math.Sin(-2.614 + 16.6187 * num2)) * (180.0 / Math.PI) % 360.0 / (180.0 / Math.PI);
			double num4 = -0.023 + num3 * (0.495 + num3 * (0.05 - 0.401 * num3)) + 0.033 * Math.Sin(-0.924 + 0.0633 * num2) + 0.022 * Math.Sin(-1.655 + 1.0352 * num2) + 0.008 * Math.Sin(-2.002 + 7.7786 * num2);
			double num5 = -0.2 + num3 * (-0.176 + num3 * (0.538 + 0.323 * num3)) + 0.049 * Math.Sin(0.011 + 0.0633 * num2) + 0.022 * Math.Sin(3.046 + 1.0364 * num2) + 0.008 * Math.Sin(-0.364 + 7.7771 * num2);
			double num6 = 0.075 + num3 * (0.411 + num3 * (0.353 - 0.08 * num3)) + 0.018 * Math.Sin(0.983 + 0.0633 * num2);
			double num7 = 0.225 + num3 * (0.285 + num3 * (-0.505 - 0.337 * num3));
			N = Math.Atan2(num7, num6);
			J = 2.0 * Math.Asin(Math.Sqrt(num7 * num7 + num6 * num6));
			e = Math.Sqrt(num5 * num5 + num4 * num4);
			CurlyPi = Math.Atan2(num5, num4);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre - N;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			JupiterVI_XIII_XYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && JVI_XIII_AccurateXYZ(num + 2451545.0, 7, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jdInit, 5, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(10.07 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Pasiphae(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			double num3 = num2 / 100.0;
			MeanDailyMotion = 0.008451409347022588;
			SemiMajorAxis = 0.157;
			L = (2.3711015393 + 3.086877264 * num2 + 0.011 + num3 * (-0.021 + num3 * (-0.331 - 0.397 * num3)) + 0.134 * Math.Sin(-1.23 + 0.0704 * num2) + 0.01 * Math.Sin(0.012 + 0.1324 * num2) + 0.013 * Math.Sin(2.988 + 0.5292 * num2) + 0.021 * Math.Sin(-3.101 + 0.928 * num2) + 0.044 * Math.Sin(0.556 + 0.9488 * num2) + 0.036 * Math.Sin(-2.224 + 0.9622 * num2) + 0.084 * Math.Sin(-0.716 + 1.0588 * num2) + 0.009 * Math.Sin(-0.521 + 1.5899 * num2) + 0.008 * Math.Sin(2.547 + 2.0267 * num2) + 0.013 * Math.Sin(-1.43 + 3.0861 * num2) + 0.012 * Math.Sin(-1.708 + 3.2426 * num2) + 0.016 * Math.Sin(-1.741 + 3.9922 * num2) + 0.088 * Math.Sin(1.026 + 4.1453 * num2) + 0.011 * Math.Sin(1.212 + 4.6751 * num2) + 0.034 * Math.Sin(2.763 + 7.2315 * num2) + 0.009 * Math.Sin(1.346 + 10.3176 * num2)) * (180.0 / Math.PI) % 360.0 / (180.0 / Math.PI);
			double num4 = -0.328 + num3 * (-0.001 + num3 * (0.026 + 0.028 * num3)) + 0.024 * Math.Sin(-0.819 + 0.0829 * num2) + 0.1 * Math.Sin(0.703 + 0.1533 * num2) + 0.02 * Math.Sin(0.977 + 0.9056 * num2) + 0.009 * Math.Sin(-2.065 + 0.9761 * num2) + 0.097 * Math.Sin(-2.876 + 1.0604 * num2) + 0.01 * Math.Sin(-2.705 + 1.5907 * num2) + 0.009 * Math.Sin(-1.504 + 2.025 * num2) + 0.032 * Math.Sin(-1.011 + 4.1465 * num2);
			double num5 = -0.221 + num3 * (0.012 + num3 * (-0.087 - 0.107 * num3)) + 0.038 * Math.Sin(2.545 + 0.0872 * num2) + 0.009 * Math.Sin(0.605 + 0.1223 * num2) + 0.086 * Math.Sin(2.07 + 0.1549 * num2) + 0.008 * Math.Sin(1.213 + 0.8899 * num2) + 0.096 * Math.Sin(-1.294 + 1.0602 * num2) + 0.012 * Math.Sin(-1.64 + 1.4473 * num2) + 0.008 * Math.Sin(1.805 + 1.4579 * num2) + 0.01 * Math.Sin(-1.108 + 1.5897 * num2) + 0.01 * Math.Sin(-1.409 + 3.0861 * num2) + 0.029 * Math.Sin(0.326 + 4.1466 * num2) + 0.009 * Math.Sin(-0.277 + 7.2312 * num2);
			double num6 = -0.249 + num3 * (-0.54 + num3 * (0.12 + 1.001 * num3)) + 0.378 * Math.Sin(0.682 + 0.0633 * num2) + 0.069 * Math.Sin(1.604 + 0.107 * num2) + 0.012 * Math.Sin(-2.667 + 0.2186 * num2) + 0.025 * Math.Sin(0.658 + 0.9826 * num2);
			double num7 = -0.013 + num3 * (-0.012 + num3 * (0.087 + 0.107 * num3)) + 0.274 * Math.Sin(-0.96 + 0.0784 * num2) + 0.017 * Math.Sin(1.479 + 0.1538 * num2) + 0.01 * Math.Sin(1.695 + 0.2307 * num2) + 0.021 * Math.Sin(-1.448 + 0.9828 * num2);
			N = Math.Atan2(num7, num6);
			J = 2.0 * Math.Atan(1.0 / Math.Sqrt(num7 * num7 + num6 * num6));
			e = Math.Sqrt(num5 * num5 + num4 * num4);
			CurlyPi = Math.Atan2(num5, num4);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre + N;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			JupiterVI_XIII_XYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && JVI_XIII_AccurateXYZ(num + 2451545.0, 8, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jdInit, 5, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(10.33 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Sinope(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			double num3 = num2 / 100.0;
			MeanDailyMotion = 0.00828060239890486;
			SemiMajorAxis = 0.158;
			L = (3.6591065118 + 3.0244900262 * num2 + 0.007 + num3 * (-0.003 + num3 * (-0.191 - 0.225 * num3)) + 0.098 * Math.Sin(-1.259 + 0.0652 * num2) + 0.017 * Math.Sin(-2.761 + 0.5295 * num2) + 0.009 * Math.Sin(-1.231 + 0.9924 * num2) + 0.032 * Math.Sin(-0.511 + 1.0553 * num2) + 0.063 * Math.Sin(-0.718 + 4.0819 * num2) + 0.05 * Math.Sin(-0.929 + 7.1073 * num2) + 0.008 * Math.Sin(-0.429 + 7.6368 * num2) + 0.009 * Math.Sin(2.006 + 10.1326 * num2)) * (180.0 / Math.PI) % 360.0 / (180.0 / Math.PI);
			double num4 = 0.199 + num3 * (0.031 + num3 * (-0.111 - 0.13 * num3)) + 0.037 * Math.Sin(-2.791 + 0.1388 * num2) + 0.009 * Math.Sin(-2.064 + 0.9159 * num2) + 0.07 * Math.Sin(0.38 + 1.0591 * num2) + 0.008 * Math.Sin(0.745 + 1.5889 * num2) + 0.012 * Math.Sin(2.387 + 3.0232 * num2) + 0.012 * Math.Sin(-0.228 + 3.9589 * num2) + 0.025 * Math.Sin(-2.938 + 3.9733 * num2) + 0.017 * Math.Sin(0.486 + 3.9833 * num2) + 0.041 * Math.Sin(0.214 + 4.0844 * num2);
			double num5 = 0.16 + num3 * (-0.03 + num3 * (0.083 + 0.102 * num3)) + 0.014 * Math.Sin(-0.259 + 0.087 * num2) + 0.008 * Math.Sin(-2.105 + 0.1109 * num2) + 0.036 * Math.Sin(-1.19 + 0.1441 * num2) + 0.07 * Math.Sin(1.951 + 1.0589 * num2) + 0.009 * Math.Sin(2.409 + 1.5883 * num2) + 0.012 * Math.Sin(0.24 + 3.0233 * num2) + 0.042 * Math.Sin(1.686 + 4.0844 * num2);
			double num6 = -0.212 + num3 * (-0.217 + num3 * (-0.042 + 0.315 * num3)) + 0.238 * Math.Sin(0.535 + 0.0633 * num2) + 0.027 * Math.Sin(1.454 + 0.1079 * num2) + 0.016 * Math.Sin(0.71 + 0.9886 * num2);
			double num7 = -0.013 + num3 * (-0.008 + num3 * (0.03 + 0.047 * num3)) + 0.204 * Math.Sin(-1.016 + 0.0717 * num2) + 0.008 * Math.Sin(0.886 + 0.1361 * num2) + 0.015 * Math.Sin(-1.088 + 0.9886 * num2);
			N = Math.Atan2(num7, num6);
			J = 2.0 * Math.Atan(1.0 / Math.Sqrt(num7 * num7 + num6 * num6));
			e = Math.Sqrt(num5 * num5 + num4 * num4);
			CurlyPi = Math.Atan2(num5, num4);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre + N;
			JupiterVI_XIII_XYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && JVI_XIII_AccurateXYZ(num + 2451545.0, 9, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jdInit, 5, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(11.6 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Lysithea(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			double num3 = num2 / 100.0;
			MeanDailyMotion = 0.024236827253935663;
			SemiMajorAxis = 0.078;
			L = (0.4612260951 + 8.8525011545 * num2 + 0.006 + num3 * (-0.078 + num3 * (-0.02 + 0.13 * num3)) + 0.016 * Math.Sin(-1.604 + 1.1075 * num2) + 0.009 * Math.Sin(2.851 + 16.6483 * num2)) * (180.0 / Math.PI) % 360.0 / (180.0 / Math.PI);
			double num4 = 0.07 + num3 * (-0.195 + num3 * (-0.217 + 0.05 * num3)) + 0.018 * Math.Sin(-1.403 + 0.0633 * num2) + 0.012 * Math.Sin(1.806 + 1.031 * num2);
			double num5 = 0.092 + num3 * (0.163 + num3 * (-0.235 - 0.181 * num3)) + 0.016 * Math.Sin(0.894 + 0.0633 * num2) + 0.011 * Math.Sin(0.302 + 1.0318 * num2);
			double num6 = 0.423 + num3 * (0.095 + num3 * (-0.496 - 0.206 * num3)) + 0.011 * Math.Sin(2.841 + 0.0633 * num2);
			double num7 = 0.035 + num3 * (-0.51 + num3 * (-0.071 + 0.281 * num3));
			N = Math.Atan2(num7, num6);
			J = 2.0 * Math.Asin(Math.Sqrt(num7 * num7 + num6 * num6));
			e = Math.Sqrt(num5 * num5 + num4 * num4);
			CurlyPi = Math.Atan2(num5, num4);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre - N;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			JupiterVI_XIII_XYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && JVI_XIII_AccurateXYZ(num + 2451545.0, 10, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jdInit, 5, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(11.7 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Carme(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			double num3 = num2 / 100.0;
			MeanDailyMotion = 0.00855742639507187;
			SemiMajorAxis = 0.155;
			L = (2.540558171 + 3.1255999908 * num2 - 0.009 + num3 * (-0.009 + num3 * (0.177 + 0.228 * num3)) + 0.074 * Math.Sin(1.783 + 0.065 * num2) + 0.019 * Math.Sin(-2.902 + 0.5307 * num2) + 0.03 * Math.Sin(1.317 + 1.0734 * num2) + 0.062 * Math.Sin(2.224 + 4.1918 * num2) + 0.009 * Math.Sin(2.472 + 4.7216 * num2) + 0.05 * Math.Sin(-3.113 + 7.3113 * num2) + 0.009 * Math.Sin(0.966 + 10.4308 * num2)) * (180.0 / Math.PI) % 360.0 / (180.0 / Math.PI);
			double num4 = 0.015 + num3 * (0.171 + num3 * (-0.098 - 0.121 * num3)) + 0.01 * Math.Sin(2.572 + 0.0936 * num2) + 0.018 * Math.Sin(0.903 + 0.1461 * num2) + 0.072 * Math.Sin(-1.92 + 1.0649 * num2) + 0.013 * Math.Sin(0.951 + 3.1248 * num2) + 0.04 * Math.Sin(-0.974 + 4.1837 * num2) + 0.009 * Math.Sin(3.126 + 7.3126 * num2);
			double num5 = -0.253 + num3 * (0.001 + num3 * (0.048 + 0.011 * num3)) + 0.016 * Math.Sin(2.442 + 0.1449 * num2) + 0.071 * Math.Sin(-0.346 + 1.0651 * num2) + 0.042 * Math.Sin(0.611 + 4.1852 * num2);
			double num6 = -0.202 + num3 * (-0.141 + num3 * (-0.121 + 0.146 * num3)) + 0.142 * Math.Sin(-2.349 + 0.0852 * num2) + 0.066 * Math.Sin(1.506 + 0.1168 * num2) + 0.029 * Math.Sin(-1.127 + 0.1402 * num2) + 0.009 * Math.Sin(-2.416 + 0.9856 * num2);
			double num7 = -0.011 + num3 * (-0.002 + num3 * (-0.003 + 0.003 * num3)) + 0.149 * Math.Sin(2.044 + 0.072 * num2) + 0.011 * Math.Sin(2.313 + 0.989 * num2);
			N = Math.Atan2(num7, num6);
			J = 2.0 * Math.Atan(1.0 / Math.Sqrt(num7 * num7 + num6 * num6));
			e = Math.Sqrt(num5 * num5 + num4 * num4);
			CurlyPi = Math.Atan2(num5, num4);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre + N;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			JupiterVI_XIII_XYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && JVI_XIII_AccurateXYZ(num + 2451545.0, 11, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jdInit, 5, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(11.3 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Ananke(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			double num3 = num2 / 100.0;
			MeanDailyMotion = 0.00997721114606434;
			SemiMajorAxis = 0.141;
			L = (5.8395111042 + 3.6441763711 * num2 - 0.03 + num3 * (0.093 + num3 * (0.055 - 0.112 * num3)) + 0.103 * Math.Sin(0.351 + 0.0633 * num2) + 0.013 * Math.Sin(-2.287 + 0.1007 * num2) + 0.011 * Math.Sin(-2.827 + 0.5275 * num2) + 0.025 * Math.Sin(0.852 + 1.0353 * num2) + 0.008 * Math.Sin(1.557 + 3.7418 * num2) + 0.013 * Math.Sin(1.929 + 4.6012 * num2) + 0.04 * Math.Sin(2.139 + 4.6904 * num2) + 0.034 * Math.Sin(-2.862 + 8.3474 * num2)) * (180.0 / Math.PI) % 360.0 / (180.0 / Math.PI);
			double num4 = 0.041 + num3 * (0.244 + num3 * (-0.008 + 0.079 * num3)) + 0.085 * Math.Sin(0.479 + 0.1097 * num2) + 0.026 * Math.Sin(-2.229 + 0.1466 * num2) + 0.011 * Math.Sin(1.426 + 0.1738 * num2) + 0.017 * Math.Sin(0.987 + 0.9658 * num2) + 0.048 * Math.Sin(1.012 + 1.0451 * num2) + 0.027 * Math.Sin(2.284 + 4.703 * num2);
			double num5 = 0.243 + num3 * (-0.037 + num3 * (-0.295 - 0.088 * num3)) + 0.092 * Math.Sin(1.918 + 0.1004 * num2) + 0.017 * Math.Sin(2.364 + 0.9575 * num2) + 0.05 * Math.Sin(2.543 + 1.0454 * num2) + 0.028 * Math.Sin(-2.46 + 4.7013 * num2);
			double num6 = -0.149 + num3 * (0.09 + num3 * (-0.53 - 0.564 * num3)) + 0.228 * Math.Sin(2.034 + 0.0633 * num2) + 0.021 * Math.Sin(-0.713 + 0.1107 * num2) + 0.016 * Math.Sin(-0.617 + 1.0044 * num2);
			double num7 = -0.015 + num3 * (0.325 + num3 * (0.1 - 0.354 * num3)) + 0.257 * Math.Sin(0.38 + 0.0633 * num2) + 0.028 * Math.Sin(-2.337 + 0.1031 * num2) + 0.018 * Math.Sin(-2.265 + 0.9992 * num2);
			N = Math.Atan2(num7, num6);
			J = 2.0 * Math.Atan(1.0 / Math.Sqrt(num7 * num7 + num6 * num6));
			e = Math.Sqrt(num5 * num5 + num4 * num4);
			CurlyPi = Math.Atan2(num5, num4);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre + N;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			JupiterVI_XIII_XYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && JVI_XIII_AccurateXYZ(num + 2451545.0, 12, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jdInit, 5, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(12.2 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Leda(double jdInit, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jdInit, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jdInit - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			double num3 = num2 / 100.0;
			MeanDailyMotion = 0.026081364717864475;
			SemiMajorAxis = 0.074;
			L = (6.2395342052 + 9.5262184632 * num2 - 0.017 + num3 * (0.083 + num3 * (0.122 - 0.037 * num3))) * (180.0 / Math.PI) % 360.0 / (180.0 / Math.PI);
			double num4 = -0.089 + num3 * (-0.303 + num3 * (0.19 + 0.236 * num3)) + 0.026 * Math.Sin(-0.881 + 0.0633 * num2) + 0.015 * Math.Sin(0.542 + 1.0383 * num2);
			double num5 = 0.129 + num3 * (-0.179 + num3 * (-0.321 + 0.001 * num3)) + 0.021 * Math.Sin(1.354 + 0.0633 * num2) + 0.015 * Math.Sin(-1.096 + 1.0363 * num2);
			double num6 = 0.009 + num3 * (-0.306 + num3 * (0.343 + 0.238 * num3));
			double num7 = -0.141 + num3 * (0.36 + num3 * (0.288 - 0.13 * num3)) + 0.01 * Math.Sin(0.383 + 0.0633 * num2);
			N = Math.Atan2(num7, num6);
			J = 2.0 * Math.Asin(Math.Sqrt(num7 * num7 + num6 * num6));
			e = Math.Sqrt(num5 * num5 + num4 * num4);
			CurlyPi = Math.Atan2(num5, num4);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre - N;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			JupiterVI_XIII_XYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && JVI_XIII_AccurateXYZ(num + 2451545.0, 13, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jdInit, 5, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(13.5 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		public static void JupiterVI_XIII_XYZRotation(out double x2000, out double y2000, out double z2000)
		{
			double num = RadiusVector * Math.Cos(L);
			double num2 = RadiusVector * Math.Sin(L);
			double num3 = 0.0;
			U = L;
			double num4 = num;
			double num5 = Math.Cos(J) * num2 - Math.Sin(J) * num3;
			double num6 = Math.Sin(J) * num2 + Math.Cos(J) * num3;
			x2000 = Math.Cos(N) * num4 - Math.Sin(N) * num5;
			y2000 = Math.Sin(N) * num4 + Math.Cos(N) * num5;
			z2000 = num6;
		}

		public static bool JVI_XIII_AccurateXYZ(double JD, int Moon, out double x2000, out double y2000, out double z2000)
		{
			double[] array = new double[3];
			double[] array2 = new double[4];
			long[] array3 = new long[6] { 0L, 16032L, 31320L, 47568L, 63696L, 79296L };
			long[] array4 = new long[8] { 0L, 15456L, 30792L, 45576L, 60504L, 76296L, 91680L, 106896L };
			long[] array5 = new long[12]
			{
				0L, 13560L, 23064L, 36504L, 51168L, 62832L, 74712L, 88560L, 101328L, 110976L,
				124968L, 138480L
			};
			long[] array6 = new long[6] { 0L, 11640L, 24624L, 36528L, 49560L, 61632L };
			long[] array7 = new long[6] { 0L, 15096L, 30576L, 46368L, 61800L, 77568L };
			long[] array8 = new long[6] { 0L, 13224L, 26856L, 39888L, 52848L, 66648L };
			long[] array9 = new long[6] { 0L, 14400L, 29520L, 42192L, 56592L, 70656L };
			long[] array10 = new long[8] { 0L, 14664L, 29736L, 44496L, 59040L, 73632L, 88776L, 103632L };
			long offset = 0L;
			string[] array11 = new string[8] { "6", "7", "8", "9", "10", "11", "12", "13" };
			x2000 = (y2000 = (z2000 = 0.0));
			if (JD < 2415020.5)
			{
				return false;
			}
			double num = 9060.0;
			if (Moon == 7 || Moon == 13)
			{
				num = 6800.0;
			}
			if (Moon == 8)
			{
				num = 4520.0;
			}
			int num2 = (int)((JD - 2415020.5) / num);
			double num3 = (JD - 2415020.5) % num;
			array2[0] = 1.0;
			array2[1] = 2.0 * num3 / num - 1.0;
			double num4 = num3 - num / 2.0;
			array2[2] = array2[1] * array2[1];
			array2[3] = array2[2] * array2[1];
			switch (Moon)
			{
			case 6:
				if (num2 > array3.GetUpperBound(0))
				{
					return false;
				}
				offset = array3[num2];
				break;
			case 7:
				if (num2 > array4.GetUpperBound(0))
				{
					return false;
				}
				offset = array4[num2];
				break;
			case 8:
				if (num2 > array5.GetUpperBound(0))
				{
					return false;
				}
				offset = array5[num2];
				break;
			case 9:
				if (num2 > array6.GetUpperBound(0))
				{
					return false;
				}
				offset = array6[num2];
				break;
			case 10:
				if (num2 > array7.GetUpperBound(0))
				{
					return false;
				}
				offset = array7[num2];
				break;
			case 11:
				if (num2 > array8.GetUpperBound(0))
				{
					return false;
				}
				offset = array8[num2];
				break;
			case 12:
				if (num2 > array9.GetUpperBound(0))
				{
					return false;
				}
				offset = array9[num2];
				break;
			case 13:
				if (num2 > array10.GetUpperBound(0))
				{
					return false;
				}
				offset = array10[num2];
				break;
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\J" + array11[Moon - 6] + ".bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(offset, SeekOrigin.Begin);
			for (int i = 0; i <= 2; i++)
			{
				binaryReader.ReadDouble();
				binaryReader.ReadDouble();
				double num5 = binaryReader.ReadDouble();
				double num6 = binaryReader.ReadDouble();
				double num7 = binaryReader.ReadDouble();
				double num8 = binaryReader.ReadDouble();
				array[i] = num5 + array2[1] * num6 + array2[2] * num7 + array2[3] * num8;
				for (int j = 0; j < 3; j++)
				{
					double num9 = 0.0;
					int num10 = Convert.ToInt32(binaryReader.ReadDouble());
					for (int k = 0; k < num10; k++)
					{
						double num11 = binaryReader.ReadDouble();
						double num12 = binaryReader.ReadDouble();
						double num13 = binaryReader.ReadDouble() * num4;
						num9 += num11 * Math.Cos(num13) + num12 * Math.Sin(num13);
					}
					array[i] += num9 * array2[j];
				}
			}
			x2000 = array[0];
			y2000 = array[1];
			z2000 = array[2];
			return true;
		}

		private static void Jupiter_V_XIV_to_XVI(int MoonNumber, double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			float num = 0f;
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 5, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num2 = jd - 2450464.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num3 = (jd - 2451545.0) / 36525.0;
			switch (MoonNumber)
			{
			case 5:
				SemiMajorAxis = 0.0012125841015478088;
				MeanDailyMotion = 12.6123026502328;
				e = 0.0031;
				Gamma = 0.006789330790257942;
				L = (90.013 + 722.6317118 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (260.165 + 2.52712 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (112.321 - 2.50159 * num2) % 360.0 / (180.0 / Math.PI);
				num = 7.4f;
				Na = (358.05 - 0.009 * num3) / (180.0 / Math.PI);
				Ja = (25.510000000000005 + 0.003 * num3) / (180.0 / Math.PI);
				break;
			case 14:
				SemiMajorAxis = 0.0014833098794567738;
				MeanDailyMotion = 9.31482658470753;
				e = 0.0177;
				Gamma = 0.018675022996339326;
				L = (245.908 + 533.7002502 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (109.474 + 1.23978 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (235.937 - 1.23667 * num2) % 360.0 / (180.0 / Math.PI);
				num = 8.9f;
				Na = (358.05 - 0.009 * num3) / (180.0 / Math.PI);
				Ja = (25.510000000000005 + 0.003 * num3) / (180.0 / Math.PI);
				break;
			case 15:
				SemiMajorAxis = 0.0008616432783324837;
				MeanDailyMotion = 21.0661032951721;
				e = 0.0017;
				Gamma = 0.00047123889803846896;
				L = (332.106 + 1206.9988096 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (188.604 + 8.55986 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (167.422 - 8.14567 * num2) % 360.0 / (180.0 / Math.PI);
				num = 12.4f;
				Na = (358.05 - 0.009 * num3) / (180.0 / Math.PI);
				Ja = (25.510000000000005 + 0.003 * num3) / (180.0 / Math.PI);
				break;
			case 16:
				SemiMajorAxis = 0.0008562956086453931;
				MeanDailyMotion = 21.3149142952345;
				e = 0.0008;
				Gamma = 0.0003665191429188092;
				L = (0.306 + 1221.2546298 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (178.725 + 9.10369 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (138.069 - 8.49678 * num2) % 360.0 / (180.0 / Math.PI);
				num = 10.8f;
				Na = (358.05 - 0.009 * num3) / (180.0 / Math.PI);
				Ja = (25.510000000000005 + 0.003 * num3) / (180.0 / Math.PI);
				break;
			}
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			TrueAnomaly = L - CurlyPi + Na - Math.PI * 2.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			double num4 = Math.Sin(Gamma);
			double num5 = Math.Cos(Gamma);
			double num6 = Math.Sin(Na);
			double num7 = Math.Cos(Na);
			double num8 = Math.Sin(Ja);
			double num9 = Math.Cos(Ja);
			double num10 = Math.Sin(Theta - Na);
			double num11 = Math.Cos(Theta - Na);
			double num12 = RadiusVector * Math.Cos(L - Theta);
			double num13 = RadiusVector * Math.Sin(L - Theta);
			double num14 = 0.0;
			double num15 = num12;
			double num16 = num5 * num13 - num4 * num14;
			double num17 = num4 * num13 + num5 * num14;
			double num18 = num11 * num15 - num10 * num16;
			double num19 = num10 * num15 + num11 * num16;
			double num20 = num17;
			U = Math.Atan2(num19, num18);
			double num21 = num18;
			double num22 = num9 * num19 - num8 * num20;
			double num23 = num8 * num19 + num9 * num20;
			double num24 = num7 * num21 - num6 * num22;
			double num25 = num6 * num21 + num7 * num22;
			double num26 = num23;
			x = num24;
			y = num25;
			z = num26;
			double na = Na;
			double ja = Ja;
			double num27 = Math.Sin(ja);
			double num28 = Math.Cos(ja);
			num10 = Math.Sin(Theta);
			num11 = Math.Cos(Theta);
			double num29 = num4 * num10;
			double num30 = num5 * num27 + num4 * num28 * num11;
			double num31 = num5 * num28 - num4 * num27 * num11;
			double num32 = num4 * num28 + num5 * num27 * num11;
			double num33 = num27 * num10;
			N = Math.Atan2(num29, num30) + na;
			J = Math.Atan2(Math.Sqrt(num29 * num29 + num30 * num30), num31);
			Math.Atan2(num33, num32);
			for (U = L - Theta; U > Math.PI * 2.0; U -= Math.PI * 2.0)
			{
			}
			while (U < 0.0)
			{
				U += Math.PI * 2.0;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 5, x, y, z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)((double)num + 5.0 * Math.Log10(Distance * PlanetHeliocentricDistance));
		}

		private static void Mimas(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = (jd - 2444240.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance) / 365.25;
			double num2 = 6.34E-08;
			MeanDailyMotion = 6.6324136756901595;
			double num3 = 0.7574073 * Math.Sin(0.6863507283467701 + 0.08904538 * num) + 0.012433 * Math.Sin(2.0590347317477904 + 0.26713613 * num);
			num3 = num3 + 0.0022664 * Math.Sin(2.209691552779941 + 10.19765304 * num) + 0.0010599 * Math.Sin(4.664933478022964 + 10.10860767 * num) + 0.0010228 * Math.Sin(2.8960422811267112 + 10.28669842 * num) + 0.0007266 * Math.Sin(1.3726840034010201 + 0.17809075 * num) + 0.0005061 * Math.Sin(4.533614905102911 + 0.05765338 * num) + 0.000359 * Math.Sin(3.4317361884413304 + 0.44522688 * num) + 0.0002628 * Math.Sin(0.05445427266222308 + 0.06492496 * num) + 0.0002459 * Math.Sin(3.1222544054776957 + 0.12043737 * num) + 0.0002237 * Math.Sin(0.8369900960864007 + 10.01956229 * num) + 0.0002097 * Math.Sin(3.582375556180961 + 10.3757438 * num) + 0.000197 * Math.Sin(4.45982238432859 + 0.11316579 * num) + 0.0001276 * Math.Sin(5.446823529623903 + 5.02184135 * num) + 0.0001164 * Math.Sin(3.6779323327276505 + 5.19993211 * num);
			double num4 = 0.0359719 * Math.Sin(3.827943381936563 + num * 0.08904538) + 0.0005892 * Math.Sin(5.200627385337583 + num * 0.26713613);
			num3 += 0.0001456 * Math.Sin(0.2429672851701306 + 2428.76308172 * num + 2.0 * num3 - 2.0 * num4);
			double num5 = (0.1822485 + 2435.14429644 * num + num3) % (Math.PI * 2.0);
			double num6 = 0.0159817 * Math.Sin(6.2224653025027035 + num * 6.38121472) + 0.0073147 * Math.Sin(2.3945393738586604 + num * 6.29216934) + 0.0071114 * Math.Sin(0.6256307236698873 + num * 6.4702601);
			double num7 = 0.0118896 * Math.Sin(4.087788000973479 - num * 6.37188169) + 0.0053177 * Math.Sin(0.2598446190369158 - num * 6.46092707) + 0.0053017 * Math.Sin(4.774138729320249 - num * 6.28283631);
			double num8 = 0.0159817 * Math.Cos(6.2224653025027035 + num * 6.38121472) + 0.0073147 * Math.Cos(2.3945393738586604 + num * 6.29216934) + 0.0071114 * Math.Cos(0.6256307236698873 + num * 6.4702601);
			double num9 = 0.0118896 * Math.Cos(4.087788000973479 - num * 6.37188169) + 0.0053177 * Math.Cos(0.2598446190369158 - num * 6.46092707) + 0.0053017 * Math.Cos(4.774138729320249 - num * 6.28283631);
			double zSin = num6 + 0.0015115 * Math.Sin(4.849781299101684 + num * 6.20312396) + 0.0014622 * Math.Sin(1.3119814520166575 + num * 6.55930547) + 0.0003336 * Math.Sin(1.02183791716512 + num * 6.11407859) + 0.0003307 * Math.Sin(1.9983321803634275 + num * 6.64835085) + 0.0001607 * Math.Sin(4.012826109600323 - num * 3.81643833) + 0.0026027 * Math.Sin(0.1822472804932479 + num * 2435.14429644 + num3);
			num7 = num7 + 0.0010922 * Math.Sin(2.715086544279939 - num * 6.54997244) + 0.0010741 * Math.Sin(5.460472004374499 - num * 6.19379094) + 0.0002328 * Math.Sin(6.14682273272127 - num * 6.10474556) + 0.0002224 * Math.Sin(5.170345922815481 - num * 6.63901782);
			num8 = num8 + 0.0015115 * Math.Cos(4.849781299101684 + num * 6.20312396) + 0.0014622 * Math.Cos(1.3119814520166575 + num * 6.55930547) + 0.0003336 * Math.Cos(1.02183791716512 + num * 6.11407859) + 0.0003307 * Math.Cos(1.9983321803634275 + num * 6.64835085) + 0.0001607 * Math.Cos(4.012826109600323 - num * 3.81643833) + 0.0026027 * Math.Cos(0.1822472804932479 + num * 2435.14429644 + num3);
			num9 = num9 + 0.0010922 * Math.Cos(2.715086544279939 - num * 6.54997244) + 0.0010741 * Math.Cos(5.460472004374499 - num * 6.19379094) + 0.0002328 * Math.Cos(6.14682273272127 - num * 6.10474556) + 0.0002224 * Math.Cos(5.170345922815481 - num * 6.63901782);
			e_pi_gamma_theta(zSin, num8, num7, num9, ref e, ref CurlyPi, ref Gamma, ref Theta);
			TrueAnomaly_Radius(num5 - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = num5 + EquationOfCentre;
			SemiMajorAxis = Math.Pow(37930952.7 * (1.0 + num2) * 7464960000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			SaturnXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(3.3 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Enceladus(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = (jd - 2444240.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance) / 365.25;
			double num2 = 6.9E-08;
			MeanDailyMotion = 4.57110560882927;
			double num3 = 0.0044964 * Math.Sin(2.3429648944622277 + num * 0.56590952) + 0.0033546 * Math.Sin(4.597825568283781 + num * 1.61701655);
			double num4 = (0.7997717 + 1674.8672985 * num + num3) % (Math.PI * 2.0);
			double zSin = 0.0048038 * Math.Sin(3.189432128386958 + num * 2.15444222) + 0.0001098 * Math.Sin(5.532379569556666 + num * 2.72035174) + 0.0015768 * Math.Sin(0.7997796764338815 + num * 1674.8672985 + num3);
			double zCos = 0.0048038 * Math.Cos(3.189432128386958 + num * 2.15444222) + 0.0001098 * Math.Cos(5.532379569556666 + num * 2.72035174) + 0.0015768 * Math.Cos(0.7997796764338815 + num * 1674.8672985 + num3);
			double zetaSin = 0.0001281 * Math.Sin(1.983147815871077 - num * 2.65919659);
			double zetaCos = 0.0001281 * Math.Cos(1.983147815871077 - num * 2.65919659);
			e_pi_gamma_theta(zSin, zCos, zetaSin, zetaCos, ref e, ref CurlyPi, ref Gamma, ref Theta);
			TrueAnomaly_Radius(num4 - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = num4 + EquationOfCentre;
			SemiMajorAxis = Math.Pow(37930952.7 * (1.0 + num2) * 7464960000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			SaturnXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(2.1 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Tethys(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = (jd - 2444240.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance) / 365.25;
			double num2 = 1.06E-06;
			MeanDailyMotion = 3.321490073465531;
			double num3 = 0.0359719 * Math.Sin(3.827943381936563 + num * 0.08904538) + 0.0005892 * Math.Sin(5.200627385337583 + num * 0.26713613) + 0.000105 * Math.Sin(5.3512842063697335 + num * 10.19765304);
			double num4 = (5.2391094 + 1215.66392906 * num + num3) % (Math.PI * 2.0);
			double num5 = 0.0001565 * Math.Sin(4.568469130265238 + num * 1.26305641);
			double num6 = 0.007979 * Math.Sin(3.937776951764566 - num * 1.26099496) + 0.0035868 * Math.Sin(0.10983356982800316 - num * 1.35004034) + 0.0035786 * Math.Sin(4.624127680111336 - num * 1.17194958);
			double num7 = 0.0001565 * Math.Cos(4.568469130265238 + num * 1.26305641);
			double num8 = 0.007979 * Math.Cos(3.937776951764566 - num * 1.26099496) + 0.0035868 * Math.Cos(0.10983356982800316 - num * 1.35004034) + 0.0035786 * Math.Cos(4.624127680111336 - num * 1.17194958);
			double zSin = num5 + 8.68E-05 * Math.Sin(0.7405257483286741 + num * 1.17401103) + 8.17E-05 * Math.Sin(5.9411531336662575 + num * 1.44114716) + 8.1E-05 * Math.Sin(3.1957676735716976 + num * 1.08496566) + 7.08E-05 * Math.Sin(5.254802405319487 + num * 1.35210179) + 0.0010264 * Math.Sin(5.239111895344058 + num * 1215.66392906 + num3);
			num6 = num6 + 0.0007456 * Math.Sin(2.565092948363546 - num * 1.43908571) + 0.0007269 * Math.Sin(5.310478408458106 - num * 1.08290421) + 0.0001634 * Math.Sin(5.996811683512356 - num * 0.99385883) + 0.0001629 * Math.Sin(5.020334873606569 - num * 1.52813109);
			num7 = num7 + 8.68E-05 * Math.Cos(0.7405257483286741 + num * 1.17401103) + 8.17E-05 * Math.Cos(5.9411531336662575 + num * 1.44114716) + 8.1E-05 * Math.Cos(3.1957676735716976 + num * 1.08496566) + 7.08E-05 * Math.Cos(5.254802405319487 + num * 1.35210179) + 0.0010264 * Math.Cos(5.239111895344058 + num * 1215.66392906 + num3);
			num8 = num8 + 0.0007456 * Math.Cos(2.565092948363546 - num * 1.43908571) + 0.0007269 * Math.Cos(5.310478408458106 - num * 1.08290421) + 0.0001634 * Math.Cos(5.996811683512356 - num * 0.99385883) + 0.0001629 * Math.Cos(5.020334873606569 - num * 1.52813109);
			e_pi_gamma_theta(zSin, num7, num6, num8, ref e, ref CurlyPi, ref Gamma, ref Theta);
			TrueAnomaly_Radius(num4 - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = num4 + EquationOfCentre;
			SemiMajorAxis = Math.Pow(37930952.7 * (1.0 + num2) * 7464960000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			SaturnXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(0.6 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Dione(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = (jd - 2444240.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance) / 365.25;
			double num2 = 1.963E-06;
			MeanDailyMotion = 2.292859477964139;
			double num3 = 0.0001253 * Math.Sin(5.484557548052021 + num * 0.56590952) + 9.47E-05 * Math.Sin(1.456232914693989 + num * 1.61701655);
			double num4 = (1.9945926 + 838.51087036 * num + num3) % (Math.PI * 2.0);
			double zSin = 0.0022034 * Math.Sin(4.874774413990242 + num * 0.53742567) + 0.0001172 * Math.Sin(2.6819776483696063 + num * 0.00893386) + 0.0006246 * Math.Sin(1.9945971757641596 + num * 838.51087036 + num3);
			double zetaSin = 5.91E-05 * Math.Sin(3.2215112800386136) + 0.0001655 * Math.Sin(1.5566417065612224 - num * 0.53763153);
			double zCos = 0.0022034 * Math.Cos(4.874774413990242 + num * 0.53742567) + 0.0001172 * Math.Cos(2.6819776483696063 + num * 0.00893386) + 0.0006246 * Math.Cos(1.9945971757641596 + num * 838.51087036 + num3);
			double zetaCos = 5.91E-05 * Math.Cos(3.2215112800386136) + 0.0001655 * Math.Cos(1.5566417065612224 - num * 0.53763153);
			e_pi_gamma_theta(zSin, zCos, zetaSin, zetaCos, ref e, ref CurlyPi, ref Gamma, ref Theta);
			TrueAnomaly_Radius(num4 - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = num4 + EquationOfCentre;
			SemiMajorAxis = Math.Pow(37930952.7 * (1.0 + num2) * 7464960000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			SaturnXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(0.8 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Rhea(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = (jd - 2444240.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance) / 365.25;
			double num2 = 4.32E-06;
			double num3 = 0.0014892 * Math.Sin(4.482913090332475 + num * 0.00192554) + 0.0006278 * Math.Sin(0.16416566944258665 + num * 0.00893124) + 0.0002065 * Math.Sin(2.875901181558696 + num * 0.42659824) + 0.000184 * Math.Sin(4.432542888119919 + num * 0.21329912);
			double num4 = 0.0004983 * Math.Sin(4.765132830379958 + num * 0.00192554) + 0.0003255 * Math.Sin(3.3039606339028254 + num * 0.00893124) + 6.39E-05 * Math.Sin(2.864957967148692 + num * 0.42659824) + 6.15E-05 * Math.Sin(3.666744772222367 + num * 0.17546762) + 4.69E-05 * Math.Sin(4.409574355163674 + num * 0.21329912);
			num4 = num4 + 9.27E-05 * Math.Sin(5.710682405940406 + num * 728.17054577 + 2.0 * num4 - 2.0 * num3) + 5.23E-05 * Math.Sin(4.426137529765099 + num * 364.08527289 + num4 - num3);
			double num5 = (6.2213409 + 508.00931975 * num + num4) % (Math.PI * 2.0);
			double num6 = -0.0006263 + 6.5E-05 * Math.Cos(5.710682405940406 + num * 728.17054577 + 2.0 * num4 - 2.0 * num3);
			MeanDailyMotion = (1.0 + num6) * 508.00931975 / 365.25;
			double zSin = 0.0009713 * Math.Sin(2.687929221118907 + num * 0.00893386) + 0.0001672 * Math.Sin(0.053581608036225914 + num * 0.17554922) + 0.0003116 * Math.Sin(6.221330838488908 + num * 508.00932017 + num4) + 0.0001108 * Math.Sin(3.6522410861382943 - num * 220.1612256 - num4 + 2.0 * num3);
			double zetaSin = 0.0004207 * Math.Sin(3.2214938267460935) + 0.0029705 * Math.Sin(2.6268776038841453 - num * 0.17546762) + 0.0001788 * Math.Sin(6.204558224377242 - num * 0.00893124);
			double zCos = 0.0009713 * Math.Cos(2.687929221118907 + num * 0.00893386) + 0.0001672 * Math.Cos(0.053581608036225914 + num * 0.17554922) + 0.0003116 * Math.Cos(6.221330838488908 + num * 508.00932017 + num4) + 0.0001108 * Math.Cos(3.6522410861382943 - num * 220.1612256 - num4 + 2.0 * num3);
			double zetaCos = 0.0004207 * Math.Cos(3.2214938267460935) + 0.0029705 * Math.Cos(2.6268776038841453 - num * 0.17546762) + 0.0001788 * Math.Cos(6.204558224377242 - num * 0.00893124);
			e_pi_gamma_theta(zSin, zCos, zetaSin, zetaCos, ref e, ref CurlyPi, ref Gamma, ref Theta);
			TrueAnomaly_Radius(num5 - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = num5 + EquationOfCentre;
			SemiMajorAxis = Math.Pow(37930952.7 * (1.0 + num2) * 7464960000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			SaturnXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(0.1 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Titan(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = (jd - 2444240.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance) / 365.25;
			double num2 = 0.000236638;
			double num3 = 0.0014892 * Math.Sin(4.482913090332475 + num * 0.00192554) + 0.0006278 * Math.Sin(0.16416566944258665 + num * 0.00893124);
			num3 = num3 + 0.0002065 * Math.Sin(2.875901181558696 + num * 0.42659824) + 0.000184 * Math.Sin(4.432542888119919 + num * 0.21329912) + 3.21E-05 * Math.Sin(2.4210509251964543 + num * 0.00686799) + 2.91E-05 * Math.Sin(4.13351562737573 + num * 0.63989736) + 2.78E-05 * Math.Sin(2.085877895643463 + num * 0.01786773);
			double num4 = (4.9367922 + 143.92404785 * num + num3) % (Math.PI * 2.0);
			double num5 = 0.0004983 * Math.Sin(4.765132830379958 + num * 0.00192554) + 0.0003255 * Math.Sin(3.3039606339028254 + num * 0.00893124);
			double num6 = -0.0001348 + 2.51E-05 * Math.Sin(4.426137529765099 + num * 364.08527289 + num5 - num3);
			MeanDailyMotion = (1.0 + num6) * 143.92404785 / 365.25;
			double num7 = 0.0289265 * Math.Sin(2.687597608561028 + num * 0.00893386) + 0.0001921 * Math.Sin(0.6049834786187944 - num * 0.00893386);
			double num8 = 0.0056024 * Math.Sin(3.2214938267460935) + 0.0027899 * Math.Sin(6.204697850717401 - num * 0.00893124);
			double num9 = 0.0289265 * Math.Cos(2.687597608561028 + num * 0.00893386) + 0.0001921 * Math.Cos(0.6049834786187944 - num * 0.00893386);
			double num10 = 0.0056024 * Math.Cos(3.2214938267460935) + 0.0027899 * Math.Cos(6.204697850717401 - num * 0.00893124);
			double zSin = num7 + 7.45E-05 * Math.Sin(3.484549851606679 + num * 0.41766438) + 2.43E-05 * Math.Sin(4.497032803981109 + num * 0.00700832) + 2.39E-05 * Math.Sin(4.004675421993509 + num * 0.01085941) + 1.72E-05 * Math.Sin(3.423184075106558 + num * 0.00197469) + 6.69E-05 * Math.Sin(4.936785962313601 + num * 143.92404729 + num3);
			num8 = num8 + 0.0001312 * Math.Sin(5.044263337651411 - num * 0.00192554) + 0.0001126 * Math.Sin(6.084200319159713 + num * 0.42659824) + 1.92E-05 * Math.Sin(5.094982605714367 - num * 0.21329912);
			num9 = num9 + 7.45E-05 * Math.Cos(3.484549851606679 + num * 0.41766438) + 2.43E-05 * Math.Cos(4.497032803981109 + num * 0.00700832) + 2.39E-05 * Math.Cos(4.004675421993509 + num * 0.01085941) + 1.72E-05 * Math.Cos(3.423184075106558 + num * 0.00197469) + 6.69E-05 * Math.Cos(4.936785962313601 + num * 143.92404729 + num3);
			num10 = num10 + 0.0001312 * Math.Cos(5.044263337651411 - num * 0.00192554) + 0.0001126 * Math.Cos(6.084200319159713 + num * 0.42659824) + 1.92E-05 * Math.Cos(5.094982605714367 - num * 0.21329912);
			e_pi_gamma_theta(zSin, num9, num8, num10, ref e, ref CurlyPi, ref Gamma, ref Theta);
			TrueAnomaly_Radius(num4 - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = num4 + EquationOfCentre;
			SemiMajorAxis = Math.Pow(37930952.7 * (1.0 + num2) * 7464960000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			SaturnXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(-1.28 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Hyperion(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = 3E-08;
			double num3 = -0.0015747 + 0.0052692 * Math.Cos(1.8036756088885 + num * 0.00981054) - 0.0009448 * Math.Cos(1.3790195485857597 + num * 0.098733765);
			double num4 = 0.15913 * Math.Sin(1.8036756088885 + num * 0.00981054) + 0.0040425 * Math.Sin(2.325825761207643 + num * 0.0088934126) - 0.0036745 * Math.Sin(1.2815254565693563 + num * 0.0107276673) + 0.0018763 * Math.Sin(1.3790195485857597 + num * 0.098733765) - 0.001559 * Math.Sin(5.858529246876846 + num * 0.0889232251) + 0.0015341 * Math.Sin(5.761035154860443 + num * 0.0009171274) + 0.0011322 * Math.Sin(3.1827126107667794 + num * 0.108544305) + 0.0024777 * Math.Sin(2.7580565504640395 + num * 0.1974675301) + 0.0011774 * Math.Sin(4.137076099049799 + num * 0.2962012951);
			num3 = num3 - 0.0006016 * Math.Cos(5.858529246876846 + num * 0.0889232251) + 0.0005148 * Math.Cos(3.1827126107667794 + num * 0.108544305) - 0.000131 * Math.Cos(1.2815254565693563 + num * 0.0107276673) + 0.0001186 * Math.Cos(2.325825761207643 + num * 0.0088934126) + 0.0009846 * Math.Cos(2.7580565504640395 + num * 0.1974675301) + 0.0005722 * Math.Cos(4.137076099049799 + num * 0.2962012951) + 0.0003064 * Math.Cos(5.516113100928079 + num * 0.3949350601) + 0.0002329 * Math.Cos(0.6119473423342517 + num * 0.4936688251) + 0.000183 * Math.Cos(1.9909668909200113 + num * 0.5924025902) + 0.0001428 * Math.Cos(3.370003892798291 + num * 0.6911363552) + 0.0001031 * Math.Cos(4.74902344138405 + num * 0.7898701202);
			num4 = num4 - 0.0003899 * Math.Sin(2.4466374520306906 + num * 0.0096969239) + 0.0003851 * Math.Sin(1.1607312190388288 + num * 0.009924156) - 0.0003603 * Math.Sin(5.640223464037395 + num * 0.0001136161) - 0.0003109 * Math.Sin(4.054853637988346 + num * 0.0791126851) - 0.0003037 * Math.Sin(5.4110268266655 + num * 0.0294316199) - 0.0001967 * Math.Sin(3.362935309327714 + num * 2.46364E-05) - 0.0001585 * Math.Sin(3.607351217777 + num * 0.0196210799) + 0.0001444 * Math.Sin(0.8400444222773907 + num * 0.0082528098) - 0.0001337 * Math.Sin(4.98638821965528 + num * 0.1183548449) + 0.0001354 * Math.Sin(5.809555308065885 + num * 0.0097215603) - 0.0001344 * Math.Sin(4.080981216890701 + num * 0.0098995196);
			num4 = num4 - 0.0001248 * Math.Sin(2.7673242487921295 + num * 0.0113682701) - 9.34E-05 * Math.Sin(0.9636311866111092 + num * 0.0015577301) + 8.56E-05 * Math.Sin(2.277305608002201 + num * 8.89797E-05) + 7.22E-05 * Math.Sin(0.7593753042502128 + num * 0.0116447947) + 0.0007098 * Math.Sin(5.516113100928079 + num * 0.3949350601) + 0.0004277 * Math.Sin(0.6119473423342517 + num * 0.4936688251) + 0.0002883 * Math.Sin(1.9909668909200113 + num * 0.5924025902) + 0.0002445 * Math.Sin(0.9543809415755393 + num * 0.1876569901) + 0.000208 * Math.Sin(2.333400490161299 + num * 0.2863907551) + 0.0001998 * Math.Sin(3.370003892798291 + num * 0.6911363552) + 0.0001532 * Math.Sin(3.7124374920395784 + num * 0.3851245202) + 0.0001392 * Math.Sin(4.74902344138405 + num * 0.7898701202) - 0.0001347 * Math.Sin(5.940751707938299 + num * 0.306011835) - 0.0001169 * Math.Sin(4.561732159352539 + num * 0.20727807);
			num4 = num4 + 0.0001158 * Math.Sin(5.091457040625338 + num * 0.4838582852) - 0.000111 * Math.Sin(1.0366034026369921 + num * 0.4047456001) + 9.88E-05 * Math.Sin(6.1280604432623305 + num * 0.8886038852) + 9.36E-05 * Math.Sin(0.1872912820315115 + num * 0.5825920502) - 8.79E-05 * Math.Sin(2.4156229512227516 + num * 0.5034793651) + 7.63E-05 * Math.Sin(1.5663282839097912 + num * 0.6813258152) - 7.42E-05 * Math.Sin(3.7946599531010317 + num * 0.6022131301) + 7.07E-05 * Math.Sin(1.2238946846685035 + num * 0.9873376503) - 6.95E-05 * Math.Sin(3.280206702783183 + num * 0.1965504027);
			double num5 = (4.3486836 + 0.2953088139 * num + num4) % (Math.PI * 2.0);
			double num6 = 0.1030661 * Math.Sin(3.3826924364602897 - num * 0.0008924811) + 0.0244818 * Math.Sin(2.8605422841411463 + num * 2.46462E-05) - 0.0025006 * Math.Sin(5.1863680453487895 + num * 0.0089180588) - 0.0016531 * Math.Sin(1.57901682757179 - num * 0.0107030211) - 0.001122 * Math.Sin(4.76171198504605 + num * 0.0978412839) + 0.0007518 * Math.Sin(2.0036728878745302 - num * 0.0996262462);
			double num7 = 0.0049552 * Math.Sin(3.221563639916173) + 0.0059485 * Math.Sin(3.8645080297658443 - num * 0.0001136161);
			double num8 = 0.1030661 * Math.Cos(3.3826924364602897 - num * 0.0008924811) + 0.0244818 * Math.Cos(2.8605422841411463 + num * 2.46462E-05) - 0.0025006 * Math.Cos(5.1863680453487895 + num * 0.0089180588) - 0.0016531 * Math.Cos(1.57901682757179 - num * 0.0107030211) - 0.001122 * Math.Cos(4.76171198504605 + num * 0.0978412839) + 0.0007518 * Math.Cos(2.0036728878745302 - num * 0.0996262462);
			double num9 = 0.0049552 * Math.Cos(3.221563639916173) + 0.0059485 * Math.Cos(3.8645080297658443 - num * 0.0001136161);
			num6 = num6 + 0.000258 * Math.Sin(3.8073484967630304 - num * 0.0898157062) - 0.0001702 * Math.Sin(0.19997982569351028 - num * 0.1094367861) - 0.000163 * Math.Sin(0.28220228675496317 + num * 0.1076518239) + 0.0001502 * Math.Sin(2.9580363761575494 + num * 0.0880307439) + 0.0001081 * Math.Sin(4.6642178930296465 + num * 0.0098351862) + 8.56E-05 * Math.Sin(3.904842588779433 - num * 0.0018096085) - 7.64E-05 * Math.Sin(5.708518197667933 + num * 0.0080009315) + 0.0003778 * Math.Sin(0.6246358859962506 - num * 0.1983600112) - 0.0003775 * Math.Sin(6.140748986924329 + num * 0.1965750489) - 0.0003598 * Math.Sin(2.615620230208782 + num * 0.394042579);
			num6 = num6 - 0.0002928 * Math.Sin(1.2365832283305025 + num * 0.295308814) + 0.0002217 * Math.Sin(5.528801644590077 - num * 0.2970937762) + 0.0001404 * Math.Sin(4.149764642711798 - num * 0.3958275412) + 0.0001283 * Math.Sin(2.428311494884751 - num * 0.1885494712) - 9.84E-05 * Math.Sin(5.1041455842873376 - num * 0.2081705511) + 9.3E-05 * Math.Sin(2.7707450941260383 - num * 0.4945613063) + 8.61E-05 * Math.Sin(Math.PI * 167.0 / 500.0 - num * 0.2872832363) - 6.98E-05 * Math.Sin(3.725126035701577 - num * 0.3069043162);
			num8 = num8 + 0.000258 * Math.Cos(3.8073484967630304 - num * 0.0898157062) - 0.0001702 * Math.Cos(0.19997982569351028 - num * 0.1094367861) - 0.000163 * Math.Cos(0.28220228675496317 + num * 0.1076518239) + 0.0001502 * Math.Cos(2.9580363761575494 + num * 0.0880307439) + 0.0001081 * Math.Cos(4.6642178930296465 + num * 0.0098351862) + 8.56E-05 * Math.Cos(3.904842588779433 - num * 0.0018096085) - 7.64E-05 * Math.Cos(5.708518197667933 + num * 0.0080009315) + 0.0003778 * Math.Cos(0.6246358859962506 - num * 0.1983600112) - 0.0003775 * Math.Cos(6.140748986924329 + num * 0.1965750489) - 0.0003598 * Math.Cos(2.615620230208782 + num * 0.394042579);
			num8 = num8 - 0.0002928 * Math.Cos(1.2365832283305025 + num * 0.295308814) + 0.0002217 * Math.Cos(5.528801644590077 - num * 0.2970937762) + 0.0001404 * Math.Cos(4.149764642711798 - num * 0.3958275412) + 0.0001283 * Math.Cos(2.428311494884751 - num * 0.1885494712) - 9.84E-05 * Math.Cos(5.1041455842873376 - num * 0.2081705511) + 9.3E-05 * Math.Cos(2.7707450941260383 - num * 0.4945613063) + 8.61E-05 * Math.Cos(Math.PI * 167.0 / 500.0 - num * 0.2872832363) - 6.98E-05 * Math.Cos(3.725126035701577 - num * 0.3069043162);
			num7 = num7 + 0.0015359 * Math.Sin(6.1418136377680455 - num * 2.46364E-05) - 0.0001497 * Math.Sin(2.900876843154735 - num * 0.0016713462) - 4.91E-05 * Math.Sin(2.3787266908355917 - num * 0.0007542188);
			num9 = num9 + 0.0015359 * Math.Cos(6.1418136377680455 - num * 2.46364E-05) - 0.0001497 * Math.Cos(2.900876843154735 - num * 0.0016713462) - 4.91E-05 * Math.Cos(2.3787266908355917 - num * 0.0007542188);
			num3 -= 2.68E-05 * Math.Cos(2.14846540261998 + num * 1.0955449042);
			double num10 = num5 - 0.0002989 * Math.Sin(1.9714366565901948 + num * 0.0011679623) - 0.0002231 * Math.Sin(5.533042794672423 + num * 0.0005839811) - 4.09E-05 * Math.Sin(1.221311597375552 + num * 0.0017519434) - 2.33E-05 * Math.Sin(1.648219132413365 + num * 0.0029528601) + 1.75E-05 * Math.Sin(2.7215791690973576 + num * 0.0005839811) - 1.6E-05 * Math.Sin(2.171154682895906 + num * 0.0020357649) - 1.37E-05 * Math.Sin(4.78291773545778 + num * 0.0011679623) + 1.46E-05 * Math.Sin(0.9654288757406634 + num * 0.2962012606);
			num6 = num6 + 0.0001928 * Math.Sin(5.031435167649253 + num * 0.0020604112) + 8.23E-05 * Math.Sin(5.5543707181317945 + num * 0.0011433161) + 5.17E-05 * Math.Sin(5.354652691826082 + num * 0.0002755134) + 3.27E-05 * Math.Sin(4.2812926551420905 + num * 0.0026443923) - 1.63E-05 * Math.Sin(2.633073522728725 - num * 0.0003084677) + 4.04E-05 * Math.Sin(4.348627457684032 + num * 0.2953088117);
			num7 = num7 - 0.0001609 * Math.Sin(5.193035203091408 + num * 0.0011679623) - 2.47E-05 * Math.Sin(3.9717410590083757 - num * 0.0005839811) + 2.47E-05 * Math.Sin(2.4714560339940506 + num * 0.0005839811) - 2.21E-05 * Math.Sin(4.442910143876765 + num * 0.0017519434) + 9.5E-06 * Math.Sin(5.943177715598571 + num * 0.0005839811) - 9.1E-06 * Math.Sin(1.2501618899110183 - num * 0.0011679623);
			num8 = num8 + 0.0001928 * Math.Cos(5.031435167649253 + num * 0.0020604112) + 8.23E-05 * Math.Cos(5.5543707181317945 + num * 0.0011433161) + 5.17E-05 * Math.Cos(5.354652691826082 + num * 0.0002755134) + 3.27E-05 * Math.Cos(4.2812926551420905 + num * 0.0026443923) - 1.63E-05 * Math.Cos(2.633073522728725 - num * 0.0003084677) + 4.04E-05 * Math.Cos(4.348627457684032 + num * 0.2953088117);
			num9 = num9 - 0.0001609 * Math.Cos(5.193035203091408 + num * 0.0011679623) - 2.47E-05 * Math.Cos(3.9717410590083757 - num * 0.0005839811) + 2.47E-05 * Math.Cos(2.4714560339940506 + num * 0.0005839811) - 2.21E-05 * Math.Cos(4.442910143876765 + num * 0.0017519434) + 9.5E-06 * Math.Cos(5.943177715598571 + num * 0.0005839811) - 9.1E-06 * Math.Cos(1.2501618899110183 - num * 0.0011679623);
			MeanDailyMotion = (1.0 + num3) * 0.2953088139;
			e_pi_gamma_theta(num6, num8, num7, num9, ref e, ref CurlyPi, ref Gamma, ref Theta);
			TrueAnomaly_Radius(num10 - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = num10 + EquationOfCentre;
			SemiMajorAxis = Math.Pow(37930952.7 * (1.0 + num2) * 7464960000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			SaturnXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(4.63 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Iapetus(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = (jd - 2444240.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance) / 365.25;
			double num2 = 3.1E-06;
			double num3 = 0.0359719 * Math.Sin(3.827943381936563 + num * 0.08904538);
			double num4 = 0.0001253 * Math.Sin(5.484557548052021 + num * 0.56590952) + 9.47E-05 * Math.Sin(1.456232914693989 + num * 1.61701655);
			double num5 = 0.0004983 * Math.Sin(4.765132830379958 + num * 0.00192554) + 0.0003255 * Math.Sin(3.3039606339028254 + num * 0.00893124);
			double num6 = 0.0014892 * Math.Sin(4.482913090332475 + num * 0.00192554) + 0.0006278 * Math.Sin(0.16416566944258665 + num * 0.00893124) + 0.0002065 * Math.Sin(2.875901181558696 + num * 0.42659824) + 0.000184 * Math.Sin(4.432542888119919 + num * 0.21329912);
			double num7 = 0.1928387 * Math.Sin(1.316746200874602 + num * 0.00192554) + 0.0011977 * Math.Sin(2.9058161249378793 + num * 0.00385109) + 0.0011258 * Math.Sin(4.414129664511378 + num * 0.21329912) + 0.0007466 * Math.Sin(3.411595088873316 + num * 0.00893124);
			num7 = num7 + 0.0003004 * Math.Sin(4.18276881886701 + num * 0.42852378) + 0.00024 * Math.Sin(2.9263586502338526 + num * 0.42659824) + 0.0001785 * Math.Sin(3.897163140070658 + num * 0.00691978) + 7.39E-05 * Math.Sin(5.50325002434088 + num * 0.43044933) + 5.28E-05 * Math.Sin(6.218643031440837 + num * 0.21137358) + 4.08E-05 * Math.Sin(3.776403809125171 + num * 0.01083538) + 4.03E-05 * Math.Sin(1.0959969570823591 + num * 0.20646793) + 3.81E-05 * Math.Sin(5.449842949229853 + num * 0.6418229) + 3.62E-05 * Math.Sin(0.617759288743393 + num * 0.01282203) + 3.59E-05 * Math.Sin(2.4360607567636055 + num * 0.21522466) + 3.49E-05 * Math.Sin(4.159102154209967 + num * 0.63989736) + 2.38E-05 * Math.Sin(3.2392612785313957 + num * 0.10318689) + 2.15E-05 * Math.Sin(3.3241017334708403 + num * 0.02002818) + 2.02E-05 * Math.Sin(5.342644826572362 + num * 0.01786773);
			num7 = num7 + 1.6E-05 * Math.Sin(2.4857153739828437 + num * 0.42264886) + 1.31E-05 * Math.Sin(5.873661251491637 + num * 0.31639186) + 9.9E-06 * Math.Sin(0.46930158256875526 + num * 0.64374845) + 7.5E-06 * Math.Sin(4.08738657524552 + num * 0.22013031) + 0.0007283 * Math.Sin(4.7706655241087805 + num * 114.99552496 + num6 - num7) + 0.0001227 * Math.Sin(3.603738386225372 + num * 57.43044643 + 2.0 * num7) + 1.44E-05 * Math.Sin(3.603633666470252 + num * 57.43044643 + 2.0 * num7) + 7.3E-06 * Math.Sin(1.781160861537773 + num * 57.43237198 + 2.0 * num7) + 0.000117 * Math.Sin(2.901889134120892 + num * 0.71654053 - num6 + 5.0 * num7) + 0.0001078 * Math.Sin(4.729248860958955 + num * 0.71461498 - num6 + 5.0 * num7) + 8.88E-05 * Math.Sin(3.1507556321627637 + num * 0.72048991 - num6 + 5.0 * num7);
			num7 = num7 + 8.08E-05 * Math.Sin(0.4519530098039316 + num * 0.70958135 - num6 + 5.0 * num7) + 7.29E-05 * Math.Sin(2.2815642113770673 + num * 0.70765581 - num6 + 5.0 * num7) + 7.27E-05 * Math.Sin(1.3538344474794815 + num * 0.72241545 - num6 + 5.0 * num7) + 3.53E-05 * Math.Sin(4.936018017442723 + num * 0.71856436 - num6 + 5.0 * num7) + 3.02E-05 * Math.Sin(1.1232415467059906 + num * 0.71846607 - num6 + 5.0 * num7) + 2.33E-05 * Math.Sin(5.814424776678949 + num * 0.724341 - num6 + 5.0 * num7) + 2.19E-05 * Math.Sin(1.9952778541724374 + num * 0.70370643 - num6 + 5.0 * num7) + 1.67E-05 * Math.Sin(4.86643174016571 + num * 0.7115069 - num6 + 5.0 * num7) + 1.48E-05 * Math.Sin(5.719793024635817 + num * 0.69674725 - num6 + 5.0 * num7) + 1.4E-05 * Math.Sin(4.239753818944625 + num * 0.70262218 - num6 + 5.0 * num7);
			num7 = num7 + 1.18E-05 * Math.Sin(6.061720478394025 + num * 0.70069664 - num6 + 5.0 * num7) + 8.8E-06 * Math.Sin(1.1857941470974676 + num * 0.71851522 - num6 + 5.0 * num7) + 8.5E-06 * Math.Sin(3.014201071486727 + num * 0.71658967 - num6 + 5.0 * num7) + 6.2E-06 * Math.Sin(3.2904692387849095 + num * 0.72942115 - num6 + 5.0 * num7) + 6E-06 * Math.Sin(0.8753524330452361 + num * 0.70573027 - num6 + 5.0 * num7) + 2.64E-05 * Math.Sin(4.683468874679144 + num * 143.92207259 + num6) + 2.4E-05 * Math.Sin(2.3886227076943998 + num * 57.21714732 + 2.0 * num7) + 2.22E-05 * Math.Sin(0.7364416778790074 + num * 258.91063838 + 2.0 * num6 - num7) + 2.18E-05 * Math.Sin(6.0552104002840865 + num * 479.08079784 + num5 - num7) + 2.04E-05 * Math.Sin(3.6865717125250224 + num * 28.50389879 + num7);
			num7 = num7 + 1.77E-05 * Math.Sin(3.2581457410379744 + num * 229.99104991 + 2.0 * num6 - 2.0 * num7) + 1.38E-05 * Math.Sin(1.5688590113251828 + num * 57.85704466 + 2.0 * num7) + 1.34E-05 * Math.Sin(0.17249088997459958 + num * 57.85704466 + 2.0 * num7) + 6.8E-06 * Math.Sin(4.633220845514227 + num * 57.8589702 + 2.0 * num7) + 1.18E-05 * Math.Sin(1.8284767375593394 + num * 809.58234803 + num4 - num7) + 1.03E-05 * Math.Sin(3.120596342688301 + num * 172.85449516 + num6 + num7) + 9.5E-06 * Math.Sin(4.94305169432826 + num * 172.85256962 + num6 + num7) + 7.3E-06 * Math.Sin(5.0729914571392385 + num * 1186.73540673 + num3 - num7) + 6E-06 * Math.Sin(1.7431301371368166 + num * 86.06897732 + num6 - 2.0 * num7);
			double num8 = (0.166125 + 28.92852233 * num + num7) % (Math.PI * 2.0);
			double num9 = -0.0004932 + 0.0014226 * Math.Cos(1.6290728705189872 + num * 114.99552496 + num6 - num7);
			num9 = num9 + 1.28E-05 * Math.Cos(2.9481752658837816 + num * 114.9974505 + num6 - num7) + 1.17E-05 * Math.Cos(0.30997047515419296 + num * 114.99359941 + num6 - num7) + 0.0001039 * Math.Cos(3.603738386225372 + num * 57.43044643 + 2.0 * num7) + 1.32E-05 * Math.Cos(3.603633666470252 + num * 57.43044643 + 2.0 * num7) + 4.82E-05 * Math.Cos(4.857373481347859 + num * 86.06897732 + num6 - 2.0 * num7) + 3.79E-05 * Math.Cos(3.877894705128641 + num * 258.91063838 + 2.0 * num6 - num7) + 3.48E-05 * Math.Cos(2.9136177466942934 + num * 479.08079784 + num5 - num7) + 2.11E-05 * Math.Cos(6.169145493854277 + num * 28.92654764 + num7) + 1.18E-05 * Math.Cos(3.76731064372228 + num * 28.91958847 + num7);
			num9 = num9 + 2.04E-05 * Math.Cos(2.3886227076943998 + num * 57.21714732 + 2.0 * num7) + 1.87E-05 * Math.Cos(4.970069391149132 + num * 809.58234803 + num4 - num7) + 1.64E-05 * Math.Cos(3.120596342688301 + num * 172.85449516 + num6 + num7) + 1.53E-05 * Math.Cos(4.94305169432826 + num * 172.85256962 + num6 + num7) + 1.56E-05 * Math.Cos(3.2581457410379744 + num * 229.99104991 + 2.0 * num6 - 2.0 * num7) + 1.23E-05 * Math.Cos(0.17249088997459958 + num * 57.85704466 + 2.0 * num7) + 1.18E-05 * Math.Cos(3.3141882633195126 + num * 57.85704466 + 2.0 * num7) + 1.13E-05 * Math.Cos(1.931398803549445 + num * 1186.73540673 + num3 - num7) + 8.9E-06 * Math.Cos(3.7177781995506813 + num * 28.50389879 + num7);
			double num10 = 0.0010161 * Math.Sin(5.106850844627928) + 0.0293565 * Math.Sin(3.358641799367808 + num * 0.00197469) + 0.0009954 * Math.Sin(5.831598816518573 + num * 0.00893386) + 0.0007357 * Math.Sin(6.03583724558695 - num * 0.00197469) + 0.0006699 * Math.Sin(4.7530202453711174 - num * 0.00390023) + 0.0004152 * Math.Sin(4.743961986553267 + num * 0.00390023);
			double num11 = 0.1320165 * Math.Sin(3.2215287333311338) + 0.0679455 * Math.Sin(5.04789362249556 - num * 0.00192554) + 0.0006892 * Math.Sin(1.3980436374324978 + num * 0.00192554) + 0.0002731 * Math.Sin(3.0740309582450926 - num * 0.00893124) + 0.0002641 * Math.Sin(6.0852475167109095 + num * 0.42659824) + 0.0001817 * Math.Sin(1.1230146539032313 + num * 0.42852378);
			double num12 = 0.0010161 * Math.Cos(5.106850844627928) + 0.0293565 * Math.Cos(3.358641799367808 + num * 0.00197469) + 0.0009954 * Math.Cos(5.831598816518573 + num * 0.00893386) + 0.0007357 * Math.Cos(6.03583724558695 - num * 0.00197469) + 0.0006699 * Math.Cos(4.7530202453711174 - num * 0.00390023) + 0.0004152 * Math.Cos(4.743961986553267 + num * 0.00390023);
			double num13 = 0.1320165 * Math.Cos(3.2215287333311338) + 0.0679455 * Math.Cos(5.04789362249556 - num * 0.00192554) + 0.0006892 * Math.Cos(1.3980436374324978 + num * 0.00192554) + 0.0002731 * Math.Cos(3.0740309582450926 - num * 0.00893124) + 0.0002641 * Math.Cos(6.0852475167109095 + num * 0.42659824) + 0.0001817 * Math.Cos(1.1230146539032313 + num * 0.42852378);
			double zSin = num10 + 0.0003789 * Math.Sin(2.7452981936319607 + num * 0.42462355) + 0.0001992 * Math.Sin(4.485478724332907 + num * 0.00700832) + 0.00019 * Math.Sin(3.5175714810544116 - num * 0.00582578) + 0.0001235 * Math.Sin(0.8652993365537487 + num * 0.01085941) + 0.0001012 * Math.Sin(3.7453544017321914 - num * 0.00893386) + 6.93E-05 * Math.Sin(5.576134973904163 - num * 0.01085941) + 4.89E-05 * Math.Sin(4.014990317872796 + num * 0.63792267) + 2.08E-05 * Math.Sin(1.173873548306346 - num * 0.01278495) + 2.04E-05 * Math.Sin(4.585154477914303 + num * 0.21132443) + 1.8E-05 * Math.Sin(0.23415337244755927 + num * 0.41766438) + 1.6E-05 * Math.Sin(6.234438261171385 + num * 0.21329912) + 1.34E-05 * Math.Sin(4.649312781217615 + num * 0.21527381) + 1.29E-05 * Math.Sin(5.2877891281821805 - num * 0.21132443) + 1.13E-05 * Math.Sin(4.574612689232257 + num * 0.42264886) + 1.07E-05 * Math.Sin(0.0035081117965086025 + num * 0.42852378) + 6.6E-06 * Math.Sin(1.16039960648095 + num * 0.43049847) + 0.0005938 * Math.Sin(4.936785962313601 + num * 143.92404729 + num6) + 0.0002739 * Math.Sin(1.6786402212756262 - num * 86.06700263 - num6) + 0.0002533 * Math.Sin(0.1661204382048203 + num * 28.92852233 + num7) + 0.0001049 * Math.Sin(5.987177466041349 - num * 28.5019241 - num7) + 1.33E-05 * Math.Sin(5.9872821857964675 - num * 28.5019241 - num7) + 6.7E-06 * Math.Sin(1.5265522302568404 - num * 28.50384965 - num7) + 2.06E-05 * Math.Sin(0.9190903841002138 - num * 28.28862499 - num7) + 1.72E-05 * Math.Sin(0.902422489743668 + num * 287.83916071 + 2.0 * num6) + 1.65E-05 * Math.Sin(6.221330838488908 + num * 508.00932017 + num5) + 1.3E-05 * Math.Sin(4.70619306154011 - num * 57.14045499 - num6 + 3.0 * num7) + 1.25E-05 * Math.Sin(3.056123880119631 + num * 0.71851522 - num6 + 5.0 * num7) + 1.14E-05 * Math.Sin(4.87864904492967 + num * 0.71658967 - num6 + 5.0 * num7) + 1.23E-05 * Math.Sin(3.1352222018200138 - num * 28.92852233 - num7) + 1.18E-05 * Math.Sin(6.276710135654688 - num * 28.92852233 - num7) + 6.2E-06 * Math.Sin(4.957695006752493 - num * 28.93044787 - num7) + 1.15E-05 * Math.Sin(0.6282661708403988 + num * 86.35896876 + 3.0 * num7) + 1.05E-05 * Math.Sin(0.187134202398832 - num * 143.92597283 - num6) + 9.8E-06 * Math.Sin(4.64784670464594 + num * 143.92404729 - num6) + 1.05E-05 * Math.Sin(0.049567350756638956 - num * 201.06252758 - 2.0 * num6 + 3.0 * num7) + 1.03E-05 * Math.Sin(1.7926974878934556 - num * 114.99355026 - num6) + 9E-06 * Math.Sin(1.9945971757641596 + num * 838.51087036 + num4) + 8E-06 * Math.Sin(3.193673278469304 + num * 57.85506997 - 2.0 * num7) + 7.1E-06 * Math.Sin(5.707017214511218 - num * 229.98211605 - 2.0 * num6 + 2.0 * num7) + 6.3E-06 * Math.Sin(0.39409534510031957 - num * 450.15227551 - num5 + 2.0 * num7) + 5.7E-06 * Math.Sin(5.239111895344058 + num * 1215.66392906 + num3);
			num11 = num11 + 4.57E-05 * Math.Sin(3.663027220915619 - num * 0.00385109) + 4.49E-05 * Math.Sin(5.092975477074573 - num * 0.21329912) + 3.37E-05 * Math.Sin(1.0722081193776765 + num * 0.63989736) + 3.01E-05 * Math.Sin(4.323389996700193 + num * 0.21329912) + 2.87E-05 * Math.Sin(1.1304148499316873 + num * 0.00390023) + 2.83E-05 * Math.Sin(3.775426424744054 - num * 0.21522466) + 2.83E-05 * Math.Sin(3.1798153642084688 + num * 0.21137358) + 2.35E-05 * Math.Sin(2.3920610063208283 + num * 0.6418229) + 1.92E-05 * Math.Sin(1.666981421872304 + num * 0.00587493) + 1.39E-05 * Math.Sin(6.199549129424018 - num * 0.00683119) + 1.36E-05 * Math.Sin(1.5730827081150092 - num * 0.01085678) + 1.12E-05 * Math.Sin(2.124362405649938 + num * 0.01786773);
			num11 = num11 + 9.9E-06 * Math.Sin(3.02256119860378 + num * 0.21522466) + 9.9E-06 * Math.Sin(3.5680114964370473 + num * 0.00893124) + 6.6E-06 * Math.Sin(5.899195418448314 + num * 0.01090856) + 5.9E-06 * Math.Sin(2.236761609478373 - num * 0.00592407) + 2.99E-05 * Math.Sin(1.5924558628121461 - num * 114.99552496 - num6 + num7) + 1.6E-05 * Math.Sin(3.414911214452105 - num * 114.9974505 - num6 + num7) + 2.01E-05 * Math.Sin(5.0229877740696 + num * 172.85256962 + num6 + num7) + 1.08E-05 * Math.Sin(3.2005149691371217 + num * 172.85449516 + num6 + num7);
			num12 = num12 + 0.0003789 * Math.Cos(2.7452981936319607 + num * 0.42462355) + 0.0001992 * Math.Cos(4.485478724332907 + num * 0.00700832) + 0.00019 * Math.Cos(3.5175714810544116 - num * 0.00582578) + 0.0001235 * Math.Cos(0.8652993365537487 + num * 0.01085941) + 0.0001012 * Math.Cos(3.7453544017321914 - num * 0.00893386) + 6.93E-05 * Math.Cos(5.576134973904163 - num * 0.01085941) + 4.89E-05 * Math.Cos(4.014990317872796 + num * 0.63792267) + 2.08E-05 * Math.Cos(1.173873548306346 - num * 0.01278495) + 2.04E-05 * Math.Cos(4.585154477914303 + num * 0.21132443) + 1.8E-05 * Math.Cos(0.23415337244755927 + num * 0.41766438);
			num12 = num12 + 1.6E-05 * Math.Cos(6.234438261171385 + num * 0.21329912) + 1.34E-05 * Math.Cos(4.649312781217615 + num * 0.21527381) + 1.29E-05 * Math.Cos(5.2877891281821805 - num * 0.21132443) + 1.13E-05 * Math.Cos(4.574612689232257 + num * 0.42264886) + 1.07E-05 * Math.Cos(0.0035081117965086025 + num * 0.42852378) + 6.6E-06 * Math.Cos(1.16039960648095 + num * 0.43049847) + 0.0005938 * Math.Cos(4.936785962313601 + num * 143.92404729 + num6) + 0.0002739 * Math.Cos(1.6786402212756262 - num * 86.06700263 - num6) + 0.0002533 * Math.Cos(0.1661204382048203 + num * 28.92852233 + num7) + 0.0001049 * Math.Cos(5.987177466041349 - num * 28.5019241 - num7) + 1.33E-05 * Math.Cos(5.9872821857964675 - num * 28.5019241 - num7) + 6.7E-06 * Math.Cos(1.5265522302568404 - num * 28.50384965 - num7);
			num12 = num12 + 2.06E-05 * Math.Cos(0.9190903841002138 - num * 28.28862499 - num7) + 1.72E-05 * Math.Cos(0.902422489743668 + num * 287.83916071 + 2.0 * num6) + 1.65E-05 * Math.Cos(6.221330838488908 + num * 508.00932017 + num5) + 1.3E-05 * Math.Cos(4.70619306154011 - num * 57.14045499 - num6 + 3.0 * num7) + 1.25E-05 * Math.Cos(3.056123880119631 + num * 0.71851522 - num6 + 5.0 * num7) + 1.14E-05 * Math.Cos(4.87864904492967 + num * 0.71658967 - num6 + 5.0 * num7) + 1.23E-05 * Math.Cos(3.1352222018200138 - num * 28.92852233 - num7) + 1.18E-05 * Math.Cos(6.276710135654688 - num * 28.92852233 - num7) + 6.2E-06 * Math.Cos(4.957695006752493 - num * 28.93044787 - num7);
			num12 = num12 + 1.15E-05 * Math.Cos(0.6282661708403988 + num * 86.35896876 + 3.0 * num7) + 1.05E-05 * Math.Cos(0.187134202398832 - num * 143.92597283 - num6) + 9.8E-06 * Math.Cos(4.64784670464594 + num * 143.92404729 - num6) + 1.05E-05 * Math.Cos(0.049567350756638956 - num * 201.06252758 - 2.0 * num6 + 3.0 * num7) + 1.03E-05 * Math.Cos(1.7926974878934556 - num * 114.99355026 - num6) + 9E-06 * Math.Cos(1.9945971757641596 + num * 838.51087036 + num4) + 8E-06 * Math.Cos(3.193673278469304 + num * 57.85506997 - 2.0 * num7) + 7.1E-06 * Math.Cos(5.707017214511218 - num * 229.98211605 - 2.0 * num6 + 2.0 * num7) + 6.3E-06 * Math.Cos(0.39409534510031957 - num * 450.15227551 - num5 + 2.0 * num7) + 5.7E-06 * Math.Cos(5.239111895344058 + num * 1215.66392906 + num3);
			num13 = num13 + 4.57E-05 * Math.Cos(3.663027220915619 - num * 0.00385109) + 4.49E-05 * Math.Cos(5.092975477074573 - num * 0.21329912) + 3.37E-05 * Math.Cos(1.0722081193776765 + num * 0.63989736) + 3.01E-05 * Math.Cos(4.323389996700193 + num * 0.21329912) + 2.87E-05 * Math.Cos(1.1304148499316873 + num * 0.00390023) + 2.83E-05 * Math.Cos(3.775426424744054 - num * 0.21522466) + 2.83E-05 * Math.Cos(3.1798153642084688 + num * 0.21137358) + 2.35E-05 * Math.Cos(2.3920610063208283 + num * 0.6418229) + 1.92E-05 * Math.Cos(1.666981421872304 + num * 0.00587493) + 1.39E-05 * Math.Cos(6.199549129424018 - num * 0.00683119) + 1.36E-05 * Math.Cos(1.5730827081150092 - num * 0.01085678) + 1.12E-05 * Math.Cos(2.124362405649938 + num * 0.01786773);
			num13 = num13 + 9.9E-06 * Math.Cos(3.02256119860378 + num * 0.21522466) + 9.9E-06 * Math.Cos(3.5680114964370473 + num * 0.00893124) + 6.6E-06 * Math.Cos(5.899195418448314 + num * 0.01090856) + 5.9E-06 * Math.Cos(2.236761609478373 - num * 0.00592407) + 2.99E-05 * Math.Cos(1.5924558628121461 - num * 114.99552496 - num6 + num7) + 1.6E-05 * Math.Cos(3.414911214452105 - num * 114.9974505 - num6 + num7) + 2.01E-05 * Math.Cos(5.0229877740696 + num * 172.85256962 + num6 + num7) + 1.08E-05 * Math.Cos(3.2005149691371217 + num * 172.85449516 + num6 + num7);
			MeanDailyMotion = (1.0 + num9) * 28.92852233 / 365.25;
			e_pi_gamma_theta(zSin, num12, num11, num13, ref e, ref CurlyPi, ref Gamma, ref Theta);
			TrueAnomaly_Radius(num8 - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = num8 + EquationOfCentre;
			SemiMajorAxis = Math.Pow(37930952.7 * (1.0 + num2) * 7464960000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			SaturnXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(1.5 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance) + 0.85 * Math.Sin(U - UPlanet));
		}

		private static void Phoebe(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			double dLambda = 0.0;
			double dBeta = 0.0;
			double dRad = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = 0.0057756 * Distance;
			double num2 = jd - 2447892.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * num;
			double num3 = num2 / 365.25;
			SemiMajorAxis = 0.08652765;
			MeanDailyMotion = 0.656679928;
			double num4 = (390.742 + MeanDailyMotion * num2) % 360.0 / (180.0 / Math.PI);
			e = 0.16435;
			CurlyPi = (203.958 + 1.19141 * num3) % 360.0 / (180.0 / Math.PI);
			double num5 = (233.037 + 0.45631 * num3) % 360.0 / (180.0 / Math.PI);
			TrueAnomaly_Radius(num4 - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			PhoebePerturb(jd - 2433282.5 - Convert.ToDouble(!XYZFlag) * num, ref dLambda, ref dRad, ref dBeta);
			num4 = num4 + dLambda / (180.0 / Math.PI) + EquationOfCentre;
			RadiusVector = SemiMajorAxis * (UnitaryRadiusVector + dRad);
			double num6 = Math.Sin(3.049980321152611);
			double num7 = Math.Cos(3.049980321152611);
			double num8 = Math.Sin(num5);
			double num9 = Math.Cos(num5);
			double num10 = Math.Sin(0.383431383);
			double num11 = Math.Cos(0.383431383);
			double num12 = Math.Sin(0.098279491);
			double num13 = Math.Cos(0.098279491);
			double num14 = RadiusVector * Math.Cos(num4 - num5);
			double num15 = RadiusVector * Math.Sin(num4 - num5);
			double num16 = 0.0;
			double num17 = num14;
			double num18 = num7 * num15 - num6 * num16;
			double num19 = num6 * num15 + num7 * num16;
			double num20 = num9 * num17 - num8 * num18;
			double num21 = num8 * num17 + num9 * num18;
			double num22 = num19;
			U = Math.Atan2(num21, num20);
			double num23 = num20;
			double num24 = num11 * num21 - num10 * num22;
			double num25 = num10 * num21 + num11 * num22;
			Satellites.x = num13 * num23 - num12 * num24;
			Satellites.y = num12 * num23 + num13 * num24;
			Satellites.z = num25;
			if (HighAccuracy && Occult.Phoebe.Phoebe_Chebychev(jd - Convert.ToDouble(!XYZFlag) * num, out var x, out var y, out var z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			double num26 = num6 * num8;
			double num27 = num7 * 0.37410488352396 + num6 * 0.927386400656989 * num9;
			double num28 = num7 * 0.927386400656989 - num6 * 0.37410488352396 * num9;
			double num29 = num6 * 0.927386400656989 + num7 * 0.37410488352396 * num9;
			double num30 = 0.37410488352396 * num8;
			N = Math.Atan2(num26, num27);
			J = Math.Atan2(Math.Sqrt(num26 * num26 + num27 * num27), num28);
			double num31 = Math.Atan2(num30, num29);
			for (U = num4 - num5 + num31; U > Math.PI * 2.0; U -= Math.PI * 2.0)
			{
			}
			while (U < 0.0)
			{
				U += Math.PI * 2.0;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jd - Convert.ToDouble(!XYZFlag) * num, 6, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(6.89 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void PhoebePerturb(double T, ref double dLambda, ref double dRad, ref double dBeta)
		{
			double num = (299.65 - 0.687605 * T) % 360.0 / (180.0 / Math.PI);
			double num2 = (357.707 - 0.653571 * T) % 360.0 / (180.0 / Math.PI);
			double num3 = (66.243 + 0.033444 * T) % 360.0 / (180.0 / Math.PI);
			double num4 = (54.656 - 0.655694 * T - 0.000626 * T * T / 133408.0) % 360.0 / (180.0 / Math.PI);
			dLambda = 0.469 * Math.Sin(num3) + 0.02 * Math.Sin(2.0 * num3) - 0.101 * Math.Sin(num2 - num3) + 0.114 * Math.Sin(num2 + num3) - 0.02 * Math.Sin(2.0 * num2 - num3) + 0.023 * Math.Sin(2.0 * num2 + num3) - 0.005 * Math.Sin(3.0 * num2 - num3) + 0.005 * Math.Sin(3.0 * num2 + num3) - 0.104 * Math.Sin(2.0 * num4) - 0.032 * Math.Sin(2.0 * num4 + num2) - 0.008 * Math.Sin(2.0 * num4 + 2.0 * num2) - 0.056 * Math.Sin(2.0 * num4 - num2) - 0.142 * Math.Sin(2.0 * num) - 0.008 * Math.Sin(2.0 * num - num3) + 0.014 * Math.Sin(2.0 * num + num3) - 0.023 * Math.Sin(2.0 * num + num2) - 0.005 * Math.Sin(2.0 * num + 2.0 * num2) - 0.172 * Math.Sin(2.0 * num - num2 - num3) - 0.016 * Math.Sin(2.0 * num - num2 - 2.0 * num3) + 0.088 * Math.Sin(2.0 * num - num2 + num3) - 0.181 * Math.Sin(2.0 * num - 2.0 * num2) - 0.023 * Math.Sin(2.0 * num - 2.0 * num2 - num3) + 0.012 * Math.Sin(2.0 * num - 2.0 * num2 + num3) - 0.034 * Math.Sin(2.0 * num - 3.0 * num2) - 0.011 * Math.Sin(2.0 * num - 2.0 * num4) + 0.012 * Math.Sin(4.0 * num - 2.0 * num2) + 0.04 * Math.Sin(num) + 0.098 * Math.Sin(num + num3) + 0.008 * Math.Sin(num + num2) + 0.02 * Math.Sin(num + num2 + num3) + 0.018 * Math.Sin(num - num2) + 0.025 * Math.Sin(num - num2 + num3);
			dRad = 0.00043 - 0.00022 * Math.Cos(num3) - 0.00017 * Math.Cos(num2) - 0.00093 * Math.Cos(num2 - num3) + 0.00098 * Math.Cos(num2 + num3) - 0.00026 * Math.Cos(2.0 * num2 - num3) + 0.00039 * Math.Cos(2.0 * num2 + num3) - 0.00011 * Math.Cos(3.0 * num2 - num3) + 0.00011 * Math.Cos(3.0 * num2 + num3) - 0.00026 * Math.Cos(2.0 * num4) - 6E-05 * Math.Cos(2.0 * num4 + num2) - 0.00077 * Math.Cos(2.0 * num4 - num2) - 0.00226 * Math.Cos(2.0 * num) - 0.0001 * Math.Cos(2.0 * num - num3) + 0.0002 * Math.Cos(2.0 * num + num3) - 0.00065 * Math.Cos(2.0 * num + num2) - 0.00016 * Math.Cos(2.0 * num + 2.0 * num2) - 0.00164 * Math.Cos(2.0 * num - num2 - num3) - 0.00015 * Math.Cos(2.0 * num - num2 - 2.0 * num3) + 0.00076 * Math.Cos(2.0 * num - num2 + num3) - 0.00021 * Math.Cos(2.0 * num - 2.0 * num2) - 5E-05 * Math.Cos(2.0 * num - 2.0 * num2 - num3) + 0.00025 * Math.Cos(2.0 * num - 3.0 * num2) + 0.00012 * Math.Cos(2.0 * num - 4.0 * num2) + 0.00015 * Math.Cos(4.0 * num - 2.0 * num2) + 0.0004 * Math.Cos(num) + 0.00089 * Math.Cos(num + num3) + 0.0001 * Math.Cos(num + num2) + 0.00026 * Math.Cos(num + num2 + num3) - 0.01343 * Math.Cos(2.0 * num - num2);
			dBeta = -0.007 * Math.Sin(num4 - num2) + 0.011 * Math.Sin(num4 - 2.0 * num2) + 0.017 * Math.Sin(2.0 * num + num4) + 0.058 * Math.Sin(2.0 * num + num4 - num2) + 0.007 * Math.Sin(2.0 * num + num4 - num2 - num3) + 0.087 * Math.Sin(2.0 * num - num4) + 0.01 * Math.Sin(2.0 * num - num4 - num3) + 0.016 * Math.Sin(2.0 * num - num4 + num3) + 0.048 * Math.Sin(2.0 * num - num4 - num2) + 0.006 * Math.Sin(2.0 * num - num4 - num2 - num3) + 0.011 * Math.Sin(2.0 * num - num4 - 2.0 * num2);
		}

		private static void Helene(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = 2.29427177;
			double num3 = -0.00802443;
			double num4 = 2.29714724;
			double num5 = 2.29571726;
			double num6 = 3.27342548;
			double num7 = 1.30770422;
			double num8 = 0.77232982;
			double num9 = 3.07410251;
			double[] array = new double[3];
			double[] array2 = new double[3];
			int[,] array3 = new int[24, 5]
			{
				{ 0, 0, 0, 0, 1 },
				{ 1, 0, 0, 0, 1 },
				{ 1, 1, 0, 0, 1 },
				{ 0, 0, 1, 0, 1 },
				{ 1, 0, 1, 0, 1 },
				{ 0, 0, 2, 0, 1 },
				{ 1, 0, 2, 0, 1 },
				{ 0, 0, 3, 0, 1 },
				{ 0, 1, 0, 0, -1 },
				{ 1, 1, 0, 0, -1 },
				{ 0, 0, 1, 0, -1 },
				{ 1, 0, 1, 0, -1 },
				{ 0, 0, 2, 0, -1 },
				{ 1, 0, 2, 0, -1 },
				{ 0, 0, 3, 0, -1 },
				{ 1, 0, 0, 1, 0 },
				{ 0, 1, 0, 0, 1 },
				{ 1, 0, 3, 0, 1 },
				{ 1, 0, 3, 0, -1 },
				{ 1, 1, 1, 0, 1 },
				{ 1, 1, -1, 0, 1 },
				{ 0, 0, 0, 1, 0 },
				{ 0, 1, 1, 0, 1 },
				{ 0, 1, -1, 0, 1 }
			};
			double[,] array4 = new double[24, 6]
			{
				{ -0.002396, -0.000399, 0.000442, 0.001278, -0.004939, 0.002466 },
				{ 0.000557, -0.002152, 0.001074, 0.0055, 0.000916, -0.001015 },
				{ -3E-06, 0.0, 0.0, 3E-06, -1.1E-05, 6E-06 },
				{ -6.6E-05, 0.000265, -0.000133, -0.000676, -0.000107, 0.000122 },
				{ -0.000295, -4.7E-05, 5.3E-05, 0.000151, -0.000607, 0.000303 },
				{ 1.5E-05, 1.7E-05, -1E-05, -4.4E-05, 3.3E-05, -1.3E-05 },
				{ -1.9E-05, 1.4E-05, -6E-06, -3.5E-05, -3.8E-05, 2.3E-05 },
				{ 2E-06, 0.0, 0.0, -2E-06, 4E-06, -2E-06 },
				{ -2E-06, 8E-06, -4E-06, 0.0, 0.0, 0.0 },
				{ 9E-06, 0.0, -2E-06, 0.0, 0.0, 0.0 },
				{ -6.7E-05, 0.000264, -0.000132, -0.000677, -0.00011, 0.000123 },
				{ 0.000294, 4.8E-05, -5.3E-05, -0.000154, 0.000608, -0.000304 },
				{ 1.5E-05, 1.6E-05, -1E-05, -4.4E-05, 3.3E-05, -1.3E-05 },
				{ 1.9E-05, -1.4E-05, 6E-06, 3.5E-05, 3.8E-05, -2.3E-05 },
				{ 2E-06, 0.0, 0.0, -2E-06, 4E-06, -2E-06 },
				{ 0.0, 5E-06, 1E-05, 0.0, 0.0, 0.0 },
				{ 0.0, 2E-06, 0.0, -1.3E-05, -2E-06, 2E-06 },
				{ 0.0, 2E-06, 0.0, -4E-06, -2E-06, 0.0 },
				{ 0.0, -2E-06, 0.0, 4E-06, 2E-06, 0.0 },
				{ 0.0, 0.0, 0.0, -3E-06, 0.0, 0.0 },
				{ 0.0, 0.0, 0.0, -3E-06, 0.0, 0.0 },
				{ 0.0, 0.0, 0.0, 0.0, 5E-06, 1E-05 },
				{ 0.0, 0.0, 0.0, 0.0, 3E-06, 0.0 },
				{ 0.0, 0.0, 0.0, 0.0, 3E-06, 0.0 }
			};
			for (int i = 0; i < 24; i++)
			{
				double num10 = (double)array3[i, 1] * (num2 * num + num6) + (double)array3[i, 2] * (num3 * num + num7) + (double)array3[i, 3] * (num4 * num + num8) + (double)array3[i, 4] * (num5 * num + num9);
				double num11 = ((array3[i, 0] != 0) ? Math.Sin(num10) : Math.Cos(num10));
				double num12 = 365.25 * num11;
				for (int j = 0; j < 3; j++)
				{
					array[j] += array4[i, j] * num11;
					array2[j] += array4[i, j + 3] * num12;
				}
			}
			double num13 = 0.39777715575;
			double num14 = 0.91748206214;
			x = array[0];
			y = num14 * array[1] - num13 * array[2];
			z = num13 * array[1] + num14 * array[2];
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, x, y, z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(8.4 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Telesto(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = 3.32489098;
			double num3 = -0.00948045;
			double num4 = 3.33170385;
			double num5 = 3.32830561;
			double num6 = 6.2423359;
			double num7 = 4.62624497;
			double num8 = 0.04769409;
			double num9 = 3.24465053;
			double[] array = new double[3];
			double[] array2 = new double[3];
			int[,] array3 = new int[12, 5]
			{
				{ 1, 0, 0, 1, 0 },
				{ 0, 0, 0, 0, 1 },
				{ 1, 0, 0, 0, 1 },
				{ 1, 1, 0, 0, 1 },
				{ 0, 0, 1, 0, 1 },
				{ 1, 0, 1, 0, 1 },
				{ 1, 1, 0, 0, -1 },
				{ 0, 0, 1, 0, -1 },
				{ 1, 0, 1, 0, -1 },
				{ 0, 1, 0, 0, 1 },
				{ 0, 1, 0, 0, -1 },
				{ 0, 0, 0, 1, 0 }
			};
			double[,] array4 = new double[12, 6]
			{
				{ 2E-06, 1E-05, 1.9E-05, 0.0, 0.0, 0.0 },
				{ -0.001933, -0.000253, 0.00032, 0.001237, -0.005767, 0.002904 },
				{ 0.000372, -0.001733, 0.000873, 0.006432, 0.000842, -0.001066 },
				{ -2E-06, 0.0, 0.0, 3E-06, -1.4E-05, 7E-06 },
				{ -6E-06, 2.9E-05, -1.5E-05, -0.000108, -1.4E-05, 1.8E-05 },
				{ -3.3E-05, -4E-06, 5E-06, 2E-05, -9.7E-05, 4.9E-05 },
				{ 7E-06, 0.0, 0.0, 0.0, 0.0, 0.0 },
				{ -6E-06, 2.9E-05, -1.5E-05, -0.000108, -1.4E-05, 1.8E-05 },
				{ 3.2E-05, 4E-06, -5E-06, -2.1E-05, 9.7E-05, -4.9E-05 },
				{ 0.0, 2E-06, 0.0, -1.6E-05, -2E-06, 3E-06 },
				{ 0.0, 7E-06, -3E-06, 0.0, 0.0, 0.0 },
				{ 0.0, 0.0, 0.0, 2E-06, 1E-05, 1.9E-05 }
			};
			for (int i = 0; i < 12; i++)
			{
				double num10 = (double)array3[i, 1] * (num2 * num + num6) + (double)array3[i, 2] * (num3 * num + num7) + (double)array3[i, 3] * (num4 * num + num8) + (double)array3[i, 4] * (num5 * num + num9);
				double num11 = ((array3[i, 0] != 0) ? Math.Sin(num10) : Math.Cos(num10));
				double num12 = 365.25 * num11;
				for (int j = 0; j < 3; j++)
				{
					array[j] += array4[i, j] * num11;
					array2[j] += array4[i, j + 3] * num12;
				}
			}
			double num13 = 0.39777715575;
			double num14 = 0.91748206214;
			x = array[0];
			y = num14 * array[1] - num13 * array[2];
			z = num13 * array[1] + num14 * array[2];
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, x, y, z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(8.9 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Calypso(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jd, 6, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2451545.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = -3.32489617;
			double num3 = 0.00946761;
			double num4 = -3.33170262;
			double num5 = 3.32830561;
			double num6 = 5.4138476;
			double num7 = 1.36874776;
			double num8 = 5.64157287;
			double num9 = 3.2507488;
			double[] array = new double[3];
			double[] array2 = new double[3];
			int[,] array3 = new int[24, 5]
			{
				{ 1, 0, 0, 1, 0 },
				{ 0, 0, 0, 0, 1 },
				{ 0, 1, 0, 0, 1 },
				{ 1, 0, 0, 0, 1 },
				{ 1, 1, 0, 0, 1 },
				{ 0, 0, 1, 0, 1 },
				{ 1, 0, 1, 0, 1 },
				{ 0, 0, 2, 0, 1 },
				{ 0, 1, 0, 0, -1 },
				{ 0, 0, 1, 0, -1 },
				{ 1, 0, 1, 0, -1 },
				{ 0, 0, 2, 0, -1 },
				{ 1, 0, 2, 0, 1 },
				{ 1, 1, 0, 0, -1 },
				{ 1, 0, 2, 0, -1 },
				{ 0, 0, 1, 1, 0 },
				{ 0, 0, 1, -1, 0 },
				{ 0, 0, 0, 1, 0 },
				{ 0, 1, 1, 0, -1 },
				{ 0, 1, -1, 0, -1 },
				{ 1, 1, 1, 0, -1 },
				{ 1, 1, -1, 0, -1 },
				{ 1, 0, 1, 1, 0 },
				{ 1, 0, 1, -1, 0 }
			};
			double[,] array4 = new double[24, 6]
			{
				{ 5E-06, 2.7E-05, 5.2E-05, 0.0, 0.0, 0.0 },
				{ 0.000651, 0.001615, -0.00091, -0.006145, 0.00217, -0.000542 },
				{ -1.1E-05, 4E-06, 0.0, 0.0, 0.0, 0.0 },
				{ -0.001846, 0.000652, -0.000163, -0.002166, -0.005375, 0.00303 },
				{ -4E-06, -1E-05, 6E-06, 0.0, 0.0, 0.0 },
				{ -7.7E-05, 2.8E-05, -7E-06, -9.2E-05, -0.000225, 0.000127 },
				{ -2.8E-05, -6.7E-05, 3.8E-05, 0.000257, -9.2E-05, 2.3E-05 },
				{ -2E-06, 0.0, 0.0, 4E-06, -6E-06, 3E-06 },
				{ -4E-06, 0.0, 0.0, -9E-06, -2.2E-05, 1.2E-05 },
				{ -7.8E-05, 2.7E-05, -7E-06, -8.9E-05, -0.000225, 0.000127 },
				{ 2.7E-05, 6.8E-05, -3.8E-05, -0.000257, 8.9E-05, -2.2E-05 },
				{ -2E-06, 0.0, 0.0, 4E-06, -6E-06, 3E-06 },
				{ 0.0, -2E-06, 0.0, 7E-06, 3E-06, -2E-06 },
				{ 0.0, 3E-06, -2E-06, -2.5E-05, 9E-06, -2E-06 },
				{ 0.0, 2E-06, 0.0, -7E-06, -3E-06, 2E-06 },
				{ 0.0, 0.0, -2E-06, 0.0, 0.0, 0.0 },
				{ 0.0, 0.0, -2E-06, 0.0, 0.0, 0.0 },
				{ 0.0, 0.0, 0.0, 5E-06, 2.7E-05, 5.2E-05 },
				{ 0.0, 0.0, 0.0, 2E-06, 0.0, 0.0 },
				{ 0.0, 0.0, 0.0, 2E-06, 0.0, 0.0 },
				{ 0.0, 0.0, 0.0, 0.0, -2E-06, 0.0 },
				{ 0.0, 0.0, 0.0, 0.0, -2E-06, 0.0 },
				{ 0.0, 0.0, 0.0, 0.0, 0.0, 2E-06 },
				{ 0.0, 0.0, 0.0, 0.0, 0.0, -2E-06 }
			};
			for (int i = 0; i < 24; i++)
			{
				double num10 = (double)array3[i, 1] * (num2 * num + num6) + (double)array3[i, 2] * (num3 * num + num7) + (double)array3[i, 3] * (num4 * num + num8) + (double)array3[i, 4] * (num5 * num + num9);
				double num11 = ((array3[i, 0] != 0) ? Math.Sin(num10) : Math.Cos(num10));
				double num12 = 365.25 * num11;
				for (int j = 0; j < 3; j++)
				{
					array[j] += array4[i, j] * num11;
					array2[j] += array4[i, j + 3] * num12;
				}
			}
			double num13 = 0.39777715575;
			double num14 = 0.91748206214;
			x = array[0];
			y = num14 * array[1] - num13 * array[2];
			z = num13 * array[1] + num14 * array[2];
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 6, x, y, z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(9.1 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void SaturnXYZRotation(out double x2000, out double y2000, out double z2000)
		{
			double num = Math.Sin(Gamma);
			double num2 = Math.Cos(Gamma);
			double num3 = Math.Sin(Theta);
			double num4 = Math.Cos(Theta);
			double num5 = RadiusVector * Math.Cos(L - Theta);
			double num6 = RadiusVector * Math.Sin(L - Theta);
			double num7 = 0.0;
			double num8 = num5;
			double num9 = num2 * num6 - num * num7;
			double num10 = num * num6 + num2 * num7;
			double num11 = num4 * num8 - num3 * num9;
			double num12 = num3 * num8 + num4 * num9;
			double num13 = num10;
			double num14 = num11;
			double num15 = 0.8825277166 * num12 - 0.4702603848 * num13;
			double num16 = 0.4702603848 * num12 + 0.8825277166 * num13;
			double num17 = -0.9833473365 * num14 - 0.1817361159 * num15;
			double num18 = 0.1817361159 * num14 + -0.9833473365 * num15;
			double num19 = num16;
			x2000 = num17;
			y2000 = 0.91748206214 * num18 - 0.39777715575 * num19;
			z2000 = 0.39777715575 * num18 + 0.91748206214 * num19;
			double num20 = num * num3;
			double num21 = num2 * 0.4702603848 + num * 0.8825277166 * num4;
			double num22 = Math.Atan2(num20, num21);
			double num23 = Math.Sqrt(num20 * num20 + num21 * num21);
			double num24 = Math.Sqrt(1.0 - num23 * num23);
			double num25 = 0.4702603848 * num3;
			num21 = num * 0.8825277166 + num2 * 0.4702603848 * num4;
			double num26 = Math.Atan2(num25, num21);
			double num27 = num23 * Math.Sin(2.958840972 + num22);
			num21 = num24 * 0.39777715575 + num23 * 0.91748206214 * Math.Cos(2.958840972 + num22);
			double num28 = num24 * 0.91748206214 - num23 * 0.39777715575 * Math.Cos(2.958840972 + num22);
			N = Math.Atan2(num27, num21);
			J = Math.Atan(Math.Sqrt(num27 * num27 + num21 * num21) / num28);
			double num29 = Math.Sin(2.958840972 + num22) * 0.39777715575;
			num21 = num23 * 0.91748206214 + num24 * 0.39777715575 * Math.Cos(2.958840972 + num22);
			double num30 = Math.Atan2(num29, num21);
			for (U = L - Theta + num30 + num26; U > Math.PI * 2.0; U -= Math.PI * 2.0)
			{
			}
			while (U < 0.0)
			{
				U += Math.PI * 2.0;
			}
		}

		private static void Miranda(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 7, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2444239.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			UranianElements(num, out var N, out var N2, out var N3, out var _, out var _, out var E, out var E2, out var E3, out var E4, out var E5, out var I, out var I2, out var I3, out var I4, out var I5);
			MeanDailyMotion = 1E-06 * (4443522.67 - 34.92 * Math.Cos(N - 3.0 * N2 + 2.0 * N3) + 8.47 * Math.Cos(2.0 * N - 6.0 * N2 + 4.0 * N3) + 1.31 * Math.Cos(3.0 * N - 9.0 * N2 + 6.0 * N3) - 52.28 * Math.Cos(N - N2) - 136.65 * Math.Cos(2.0 * N - 2.0 * N2));
			L = N + 1E-06 * (25472.17 * Math.Sin(N - 3.0 * N2 + 2.0 * N3) - 3088.31 * Math.Sin(2.0 * N - 6.0 * N2 + 4.0 * N3) - 318.1 * Math.Sin(3.0 * N - 9.0 * N2 + 6.0 * N3) - 37.49 * Math.Sin(4.0 * N - 12.0 * N2 + 8.0 * N3) - 57.85 * Math.Sin(N - N2) - 62.32 * Math.Sin(2.0 * N - 2.0 * N2) - 27.95 * Math.Sin(3.0 * N - 3.0 * N2));
			double zSin = 1E-06 * (1312.38 * Math.Sin(E) + 71.81 * Math.Sin(E2) + 69.77 * Math.Sin(E3) + 6.75 * Math.Sin(E4) + 6.27 * Math.Sin(E5) - 123.31 * Math.Sin(0.0 - N + 2.0 * N2) + 39.52 * Math.Sin(-2.0 * N + 3.0 * N2) + 194.1 * Math.Sin(N));
			double zCos = 1E-06 * (1312.38 * Math.Cos(E) + 71.81 * Math.Cos(E2) + 69.77 * Math.Cos(E3) + 6.75 * Math.Cos(E4) + 6.27 * Math.Cos(E5) - 123.31 * Math.Cos(0.0 - N + 2.0 * N2) + 39.52 * Math.Cos(-2.0 * N + 3.0 * N2) + 194.1 * Math.Cos(N));
			double zetaSin = 1E-06 * (37871.71 * Math.Sin(I) + 27.01 * Math.Sin(I2) + 30.76 * Math.Sin(I3) + 12.18 * Math.Sin(I4) + 5.37 * Math.Sin(I5));
			double zetaCos = 1E-06 * (37871.71 * Math.Cos(I) + 27.01 * Math.Cos(I2) + 30.76 * Math.Cos(I3) + 12.18 * Math.Cos(I4) + 5.37 * Math.Cos(I5));
			e_pi_gamma_theta(zSin, zCos, zetaSin, zetaCos, ref e, ref CurlyPi, ref Gamma, ref Theta);
			Theta += Math.PI;
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre - Math.PI;
			SemiMajorAxis = Math.Pow(43256150406144000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			UranusXYZRotation(out var x, out var y, out var z, MainSat: true);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && AccurateUranusItoV(num + 2444239.5, 5, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 7, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(3.6 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Ariel(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 7, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2444239.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			UranianElements(num, out var N, out var N2, out var N3, out var N4, out var _, out var E, out var E2, out var E3, out var E4, out var E5, out var I, out var I2, out var I3, out var I4, out var I5);
			MeanDailyMotion = 1E-06 * (2492542.57 + 2.55 * Math.Cos(N - 3.0 * N2 + 2.0 * N3) - 42.16 * Math.Cos(N2 - N3) - 102.56 * Math.Cos(2.0 * N2 - 2.0 * N3));
			L = N2 + 1E-06 * (-1860.5 * Math.Sin(N - 3.0 * N2 + 2.0 * N3) + 219.99 * Math.Sin(2.0 * N - 6.0 * N2 + 4.0 * N3) + 23.1 * Math.Sin(3.0 * N - 9.0 * N2 + 6.0 * N3) + 4.3 * Math.Sin(4.0 * N - 12.0 * N2 + 8.0 * N3) - 90.11 * Math.Sin(N2 - N3) - 91.07 * Math.Sin(2.0 * N2 - 2.0 * N3) - 42.75 * Math.Sin(3.0 * N2 - 3.0 * N3) - 16.49 * Math.Sin(2.0 * N2 - 2.0 * N4));
			double zSin = 1E-06 * (-3.35 * Math.Sin(E) + 1187.63 * Math.Sin(E2) + 861.59 * Math.Sin(E3) + 71.5 * Math.Sin(E4) + 55.59 * Math.Sin(E5) - 84.6 * Math.Sin(0.0 - N2 + 2.0 * N3) + 91.81 * Math.Sin(-2.0 * N2 + 3.0 * N3) + 20.03 * Math.Sin(0.0 - N2 + 2.0 * N4) + 89.77 * Math.Sin(N2));
			double zCos = 1E-06 * (-3.35 * Math.Cos(E) + 1187.63 * Math.Cos(E2) + 861.59 * Math.Cos(E3) + 71.5 * Math.Cos(E4) + 55.59 * Math.Cos(E5) - 84.6 * Math.Cos(0.0 - N2 + 2.0 * N3) + 91.81 * Math.Cos(-2.0 * N2 + 3.0 * N3) + 20.03 * Math.Cos(0.0 - N2 + 2.0 * N4) + 89.77 * Math.Cos(N2));
			double zetaSin = 1E-06 * (-121.75 * Math.Sin(I) + 358.25 * Math.Sin(I2) + 290.08 * Math.Sin(I3) + 97.78 * Math.Sin(I4) + 33.97 * Math.Sin(I5));
			double zetaCos = 1E-06 * (-121.75 * Math.Cos(I) + 358.25 * Math.Cos(I2) + 290.08 * Math.Cos(I3) + 97.78 * Math.Cos(I4) + 33.97 * Math.Cos(I5));
			e_pi_gamma_theta(zSin, zCos, zetaSin, zetaCos, ref e, ref CurlyPi, ref Gamma, ref Theta);
			Theta += Math.PI;
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre - Math.PI;
			SemiMajorAxis = Math.Pow(43256760293376000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			UranusXYZRotation(out var x, out var y, out var z, MainSat: true);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && AccurateUranusItoV(num + 2444239.5, 1, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 7, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(1.45 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Umbriel(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 7, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2444239.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			UranianElements(num, out var N, out var N2, out var N3, out var N4, out var N5, out var E, out var E2, out var E3, out var E4, out var E5, out var I, out var I2, out var I3, out var I4, out var I5);
			MeanDailyMotion = 1E-06 * (1515954.9 + 9.74 * Math.Cos(N3 - 2.0 * N4 + E3) - 106.0 * Math.Cos(N2 - N3) + 54.16 * Math.Cos(2.0 * N2 - 2.0 * N3) - 23.59 * Math.Cos(N3 - N4) - 70.7 * Math.Cos(2.0 * N3 - 2.0 * N4) - 36.28 * Math.Cos(3.0 * N3 - 3.0 * N4));
			L = N3 + 1E-06 * (660.57 * Math.Sin(N - 3.0 * N2 + 2.0 * N3) - 76.51 * Math.Sin(2.0 * N - 6.0 * N2 + 4.0 * N3) - 8.96 * Math.Sin(3.0 * N - 9.0 * N2 + 6.0 * N3) - 2.53 * Math.Sin(4.0 * N - 12.0 * N2 + 8.0 * N3) - 52.91 * Math.Sin(N3 - 4.0 * N4 + 3.0 * N5) - 7.34 * Math.Sin(N3 - 2.0 * N4 + E5) - 1.83 * Math.Sin(N3 - 2.0 * N4 + E4) + 147.91 * Math.Sin(N3 - 2.0 * N4 + E3) - 7.77 * Math.Sin(N3 - 2.0 * N4 + E2) + 97.76 * Math.Sin(N2 - N3) + 73.13 * Math.Sin(2.0 * N2 - 2.0 * N3) + 34.71 * Math.Sin(3.0 * N2 - 3.0 * N3) + 18.89 * Math.Sin(4.0 * N2 - 4.0 * N3) - 67.89 * Math.Sin(N3 - N4) - 82.86 * Math.Sin(2.0 * N3 - 2.0 * N4) - 33.81 * Math.Sin(3.0 * N3 - 3.0 * N4) - 15.79 * Math.Sin(4.0 * N3 - 4.0 * N4) - 10.21 * Math.Sin(N3 - N4) - 17.08 * Math.Sin(2.0 * N3 - 2.0 * N5));
			double zSin = 1E-06 * (-0.21 * Math.Sin(E) - 227.95 * Math.Sin(E2) + 3904.69 * Math.Sin(E3) + 309.17 * Math.Sin(E4) + 221.92 * Math.Sin(E5) + 29.34 * Math.Sin(N2) + 26.2 * Math.Sin(N3) + 51.19 * Math.Sin(0.0 - N2 + 2.0 * N3) - 103.86 * Math.Sin(-2.0 * N2 + 3.0 * N3) - 27.16 * Math.Sin(-3.0 * N2 + 4.0 * N3) - 16.22 * Math.Sin(N4) + 549.23 * Math.Sin(0.0 - N3 + 2.0 * N4) + 34.7 * Math.Sin(-2.0 * N3 + 3.0 * N4) + 12.81 * Math.Sin(-3.0 * N3 + 4.0 * N4) + 21.81 * Math.Sin(0.0 - N3 + 2.0 * N5) + 46.25 * Math.Sin(N3));
			double zCos = 1E-06 * (-0.21 * Math.Cos(E) - 227.95 * Math.Cos(E2) + 3904.69 * Math.Cos(E3) + 309.17 * Math.Cos(E4) + 221.92 * Math.Cos(E5) + 29.34 * Math.Cos(N2) + 26.2 * Math.Cos(N3) + 51.19 * Math.Cos(0.0 - N2 + 2.0 * N3) - 103.86 * Math.Cos(-2.0 * N2 + 3.0 * N3) - 27.16 * Math.Cos(-3.0 * N2 + 4.0 * N3) - 16.22 * Math.Cos(N4) + 549.23 * Math.Cos(0.0 - N3 + 2.0 * N4) + 34.7 * Math.Cos(-2.0 * N3 + 3.0 * N4) + 12.81 * Math.Cos(-3.0 * N3 + 4.0 * N4) + 21.81 * Math.Cos(0.0 - N3 + 2.0 * N5) + 46.25 * Math.Cos(N3));
			double zetaSin = 1E-06 * (-10.86 * Math.Sin(I) - 81.51 * Math.Sin(I2) + 1113.36 * Math.Sin(I3) + 350.14 * Math.Sin(I4) + 106.5 * Math.Sin(I5));
			double zetaCos = 1E-06 * (-10.86 * Math.Cos(I) - 81.51 * Math.Cos(I2) + 1113.36 * Math.Cos(I3) + 350.14 * Math.Cos(I4) + 106.5 * Math.Cos(I5));
			e_pi_gamma_theta(zSin, zCos, zetaSin, zetaCos, ref e, ref CurlyPi, ref Gamma, ref Theta);
			Theta += Math.PI;
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre - Math.PI;
			SemiMajorAxis = Math.Pow(43256744616960000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			UranusXYZRotation(out var x, out var y, out var z, MainSat: true);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && AccurateUranusItoV(num + 2444239.5, 2, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 7, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(2.1 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Titania(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 7, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2444239.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			UranianElements(num, out var _, out var N2, out var N3, out var N4, out var N5, out var E, out var E2, out var E3, out var E4, out var E5, out var I, out var I2, out var I3, out var I4, out var I5);
			MeanDailyMotion = 1E-06 * (721663.16 - 2.64 * Math.Cos(N3 - 2.0 * N4 + E3) - 2.16 * Math.Cos(2.0 * N4 - 3.0 * N5 + E5) + 6.45 * Math.Cos(2.0 * N4 - 3.0 * N5 + E4) - 1.11 * Math.Cos(2.0 * N4 - 3.0 * N5 + E3) - 62.23 * Math.Cos(N2 - N4) - 56.13 * Math.Cos(N3 - N4) - 39.94 * Math.Cos(N4 - N5) - 91.85 * Math.Cos(2.0 * N4 - 2.0 * N5) - 58.31 * Math.Cos(3.0 * N4 - 3.0 * N5) - 38.6 * Math.Cos(4.0 * N4 - 4.0 * N5) - 26.18 * Math.Cos(5.0 * N4 - 5.0 * N5) - 18.06 * Math.Cos(6.0 * N4 - 6.0 * N5));
			L = N4 + 1E-06 * (20.61 * Math.Sin(N3 - 4.0 * N4 + 3.0 * N5) - 2.07 * Math.Sin(N3 - 2.0 * N4 + E5) - 2.88 * Math.Sin(N3 - 2.0 * N4 + E4) - 40.79 * Math.Sin(N3 - 2.0 * N4 + E3) + 2.11 * Math.Sin(N3 - 2.0 * N4 + E2) - 51.83 * Math.Sin(2.0 * N4 - 3.0 * N5 + E5) + 159.87 * Math.Sin(2.0 * N4 - 3.0 * N5 + E4) - 35.05 * Math.Sin(2.0 * N4 - 3.0 * N5 + E3) - 1.56 * Math.Sin(3.0 * N4 - 4.0 * N5 + E5) + 40.54 * Math.Sin(N2 - N4) + 46.17 * Math.Sin(N3 - N4) - 317.76 * Math.Sin(N4 - N5) - 305.59 * Math.Sin(2.0 * N4 - 2.0 * N5) - 148.36 * Math.Sin(3.0 * N4 - 3.0 * N5) - 82.92 * Math.Sin(4.0 * N4 - 4.0 * N5) - 49.98 * Math.Sin(5.0 * N4 - 5.0 * N5) - 31.56 * Math.Sin(6.0 * N4 - 6.0 * N5) - 20.56 * Math.Sin(7.0 * N4 - 7.0 * N5) - 13.69 * Math.Sin(8.0 * N4 - 8.0 * N5));
			double zSin = 1E-06 * (-0.02 * Math.Sin(E) - 1.29 * Math.Sin(E2) - 324.51 * Math.Sin(E3) + 932.81 * Math.Sin(E4) + 1120.89 * Math.Sin(E5) + 33.86 * Math.Sin(N2) + 17.46 * Math.Sin(N4) + 16.58 * Math.Sin(0.0 - N2 + 2.0 * N4) + 28.89 * Math.Sin(N3) - 35.86 * Math.Sin(0.0 - N3 + 2.0 * N4) - 17.86 * Math.Sin(N4) - 32.1 * Math.Sin(N5) - 177.83 * Math.Sin(0.0 - N4 + 2.0 * N5) + 793.43 * Math.Sin(-2.0 * N4 + 3.0 * N5) + 99.48 * Math.Sin(-3.0 * N4 + 4.0 * N5) + 44.83 * Math.Sin(-4.0 * N4 + 5.0 * N5) + 25.13 * Math.Sin(-5.0 * N4 + 6.0 * N5) + 15.43 * Math.Sin(-6.0 * N4 + 7.0 * N5));
			double zCos = 1E-06 * (-0.02 * Math.Cos(E) - 1.29 * Math.Cos(E2) - 324.51 * Math.Cos(E3) + 932.81 * Math.Cos(E4) + 1120.89 * Math.Cos(E5) + 33.86 * Math.Cos(N2) + 17.46 * Math.Cos(N4) + 16.58 * Math.Cos(0.0 - N2 + 2.0 * N4) + 28.89 * Math.Cos(N3) - 35.86 * Math.Cos(0.0 - N3 + 2.0 * N4) - 17.86 * Math.Cos(N4) - 32.1 * Math.Cos(N5) - 177.83 * Math.Cos(0.0 - N4 + 2.0 * N5) + 793.43 * Math.Cos(-2.0 * N4 + 3.0 * N5) + 99.48 * Math.Cos(-3.0 * N4 + 4.0 * N5) + 44.83 * Math.Cos(-4.0 * N4 + 5.0 * N5) + 25.13 * Math.Cos(-5.0 * N4 + 6.0 * N5) + 15.43 * Math.Cos(-6.0 * N4 + 7.0 * N5));
			double zetaSin = 1E-06 * (-1.43 * Math.Sin(I) - 1.06 * Math.Sin(I2) - 140.13 * Math.Sin(I3) + 685.72 * Math.Sin(I4) + 378.32 * Math.Sin(I5));
			double zetaCos = 1E-06 * (-1.43 * Math.Cos(I) - 1.06 * Math.Cos(I2) - 140.13 * Math.Cos(I3) + 685.72 * Math.Cos(I4) + 378.32 * Math.Cos(I5));
			e_pi_gamma_theta(zSin, zCos, zetaSin, zetaCos, ref e, ref CurlyPi, ref Gamma, ref Theta);
			Theta += Math.PI;
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre - Math.PI;
			SemiMajorAxis = Math.Pow(43257834501120000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			UranusXYZRotation(out var x, out var y, out var z, MainSat: true);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && AccurateUranusItoV(num + 2444239.5, 3, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 7, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(1.02 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Oberon(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 7, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2444239.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			UranianElements(num, out var _, out var N2, out var N3, out var N4, out var N5, out var _, out var E2, out var E3, out var E4, out var E5, out var I, out var I2, out var I3, out var I4, out var I5);
			MeanDailyMotion = 1E-06 * (466580.54 + 2.08 * Math.Cos(2.0 * N4 - 3.0 * N5 + E3) - 6.22 * Math.Cos(2.0 * N4 - 3.0 * N5 + E4) + 1.07 * Math.Cos(2.0 * N4 - 3.0 * N5 + E5) - 43.1 * Math.Cos(N2 - N5) - 38.94 * Math.Cos(N3 - N5) - 80.11 * Math.Cos(N4 - N5) + 59.06 * Math.Cos(2.0 * N4 - 2.0 * N5) + 37.49 * Math.Cos(3.0 * N4 - 3.0 * N5) + 24.82 * Math.Cos(4.0 * N4 - 4.0 * N5) + 16.84 * Math.Cos(5.0 * N4 - 5.0 * N5));
			L = N5 + 1E-06 * (-7.82 * Math.Sin(N3 - 4.0 * N4 + 3.0 * N5) + 51.29 * Math.Sin(2.0 * N4 - 3.0 * N5 + E5) - 158.24 * Math.Sin(2.0 * N4 - 3.0 * N5 + E4) + 34.51 * Math.Sin(2.0 * N4 - 3.0 * N5 + E3) + 47.51 * Math.Sin(N2 - N5) + 38.96 * Math.Sin(N3 - N5) + 359.73 * Math.Sin(N4 - N5) + 282.78 * Math.Sin(2.0 * N4 - 2.0 * N5) + 138.6 * Math.Sin(3.0 * N4 - 3.0 * N5) + 78.03 * Math.Sin(4.0 * N4 - 4.0 * N5) + 47.29 * Math.Sin(5.0 * N4 - 5.0 * N5) + 30.0 * Math.Sin(6.0 * N4 - 6.0 * N5) + 19.62 * Math.Sin(7.0 * N4 - 7.0 * N5) + 13.11 * Math.Sin(8.0 * N4 - 8.0 * N5));
			double zSin = 1E-06 * (-0.35 * Math.Sin(E2) + 74.53 * Math.Sin(E3) - 758.68 * Math.Sin(E4) + 1397.34 * Math.Sin(E5) + 39.0 * Math.Sin(N2) + 17.66 * Math.Sin(0.0 - N2 + 2.0 * N5) + 32.42 * Math.Sin(N3) + 79.75 * Math.Sin(N4) + 75.66 * Math.Sin(N5) + 134.04 * Math.Sin(0.0 - N4 + 2.0 * N5) - 987.26 * Math.Sin(-2.0 * N4 + 3.0 * N5) - 126.09 * Math.Sin(-3.0 * N4 + 4.0 * N5) - 57.42 * Math.Sin(-4.0 * N4 + 5.0 * N5) - 32.41 * Math.Sin(-5.0 * N4 + 6.0 * N5) - 19.99 * Math.Sin(-6.0 * N4 + 7.0 * N5) - 12.94 * Math.Sin(-7.0 * N4 + 8.0 * N5));
			double zCos = 1E-06 * (-0.35 * Math.Cos(E2) + 74.53 * Math.Cos(E3) - 758.68 * Math.Cos(E4) + 1397.34 * Math.Cos(E5) + 39.0 * Math.Cos(N2) + 17.66 * Math.Cos(0.0 - N2 + 2.0 * N5) + 32.42 * Math.Cos(N3) + 79.75 * Math.Cos(N4) + 75.66 * Math.Cos(N5) + 134.04 * Math.Cos(0.0 - N4 + 2.0 * N5) - 987.26 * Math.Cos(-2.0 * N4 + 3.0 * N5) - 126.09 * Math.Cos(-3.0 * N4 + 4.0 * N5) - 57.42 * Math.Cos(-4.0 * N4 + 5.0 * N5) - 32.41 * Math.Cos(-5.0 * N4 + 6.0 * N5) - 19.99 * Math.Cos(-6.0 * N4 + 7.0 * N5) - 12.94 * Math.Cos(-7.0 * N4 + 8.0 * N5));
			double zetaSin = 1E-06 * (-0.44 * Math.Sin(I) - 0.31 * Math.Sin(I2) + 36.89 * Math.Sin(I3) - 596.33 * Math.Sin(I4) + 451.69 * Math.Sin(I5));
			double zetaCos = 1E-06 * (-0.44 * Math.Cos(I) - 0.31 * Math.Cos(I2) + 36.89 * Math.Cos(I3) - 596.33 * Math.Cos(I4) + 451.69 * Math.Cos(I5));
			e_pi_gamma_theta(zSin, zCos, zetaSin, zetaCos, ref e, ref CurlyPi, ref Gamma, ref Theta);
			Theta += Math.PI;
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L = L + EquationOfCentre - Math.PI;
			SemiMajorAxis = Math.Pow(43257610552320000.0 / MeanDailyMotion / MeanDailyMotion, 1.0 / 3.0) / 149597871.0;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			UranusXYZRotation(out var x, out var y, out var z, MainSat: true);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (HighAccuracy && AccurateUranusItoV(num + 2444239.5, 4, out x, out y, out z))
			{
				Satellites.x = x;
				Satellites.y = y;
				Satellites.z = z;
			}
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 7, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(1.23 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void UranianElements(double Td, out double N1, out double N2, out double N3, out double N4, out double N5, out double E1, out double E2, out double E3, out double E4, out double E5, out double I1, out double I2, out double I3, out double I4, out double I5)
		{
			N1 = (-238051.58 + 4445190.55 * Td) * 1E-06 % (Math.PI * 2.0);
			N2 = (3098046.41 + 2492952.519 * Td) * 1E-06 % (Math.PI * 2.0);
			N3 = (2285401.69 + 1516148.11 * Td) * 1E-06 % (Math.PI * 2.0);
			N4 = (856358.79 + 721718.51 * Td) * 1E-06 % (Math.PI * 2.0);
			N5 = (-915591.8 + 466692.12 * Td) * 1E-06 % (Math.PI * 2.0);
			double num = Td / 365.25 / (180.0 / Math.PI);
			E1 = 0.611392 + 20.082 * num;
			E2 = 2.408974 + 6.217 * num;
			E3 = 2.067774 + 2.865 * num;
			E4 = 0.735131 + 2.078 * num;
			E5 = 0.426767 + 0.386 * num;
			I1 = 5.702313 - 20.309 * num;
			I2 = 0.395757 - 6.288 * num;
			I3 = 0.589326 - 2.836 * num;
			I4 = 1.746237 - 1.843 * num;
			I5 = 4.206896 - 0.259 * num;
		}

		private static void Inner10UranianMoons(int MoonNumber, double jd, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			float num = 0f;
			Utilities.QuickPlanet(jd, 7, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num2 = jd - 2446450.0 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			switch (MoonNumber)
			{
			case 6:
				SemiMajorAxis = 0.0003325697195249523;
				MeanDailyMotion = 18.7418126765667;
				e = 0.00026;
				Gamma = 0.001479864672765992;
				L = (70.00654 + 1074.518316 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (175.20142 + 1.502804 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (38.37431 - 1.500712 * num2) % 360.0 / (180.0 / Math.PI);
				num = 11.3f;
				break;
			case 7:
				SemiMajorAxis = 0.00035938606372279186;
				MeanDailyMotion = 16.6837549001923;
				e = 0.00992;
				Gamma = 0.0018085101709165244;
				L = (298.06836 + 956.428333 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (181.80964 + 1.145001 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (164.04843 - 1.14364 * num2) % 360.0 / (180.0 / Math.PI);
				num = 11f;
				break;
			case 8:
				SemiMajorAxis = 0.0003954972728188091;
				MeanDailyMotion = 14.4517489764368;
				e = 0.00092;
				Gamma = 0.0033698817197506514;
				L = (239.99911 + 828.387961 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (101.51355 + 0.818312 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (93.22044 - 0.81752 * num2) % 360.0 / (180.0 / Math.PI);
				num = 10.2f;
				break;
			case 9:
				SemiMajorAxis = 0.0004128850871146422;
				MeanDailyMotion = 13.548519900582;
				e = 0.00036;
				Gamma = 9.913470151327792E-05;
				L = (17.43441 + 776.582414 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (143.63916 + 0.70382 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (99.40335 - 0.703184 * num2) % 360.0 / (180.0 / Math.PI);
				num = 9.4f;
				break;
			case 10:
				SemiMajorAxis = 0.00041884529225686643;
				MeanDailyMotion = 13.2603562362602;
				e = 0.00013;
				Gamma = 0.0019638444743440195;
				L = (314.00041 + 760.055539 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (129.37318 + 0.669358 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (306.08855 - 0.668774 * num2) % 360.0 / (180.0 / Math.PI);
				num = 9.6f;
				break;
			case 11:
				SemiMajorAxis = 0.0004302081411305646;
				e = 0.00066;
				Gamma = 0.0011424925283554882;
				L = (308.67036 + 730.126135 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (63.97441 + 0.609477 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (200.15504 - 0.608971 * num2) % 360.0 / (180.0 / Math.PI);
				num = 8.8f;
				break;
			case 12:
				SemiMajorAxis = 0.00044183292555012365;
				MeanDailyMotion = 12.2390732574981;
				e = 5E-05;
				Gamma = 0.0010311405220782498;
				L = (340.8117 + 701.486481 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (122.49946 + 0.555174 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (260.0668 - 0.554737 * num2) % 360.0 / (180.0 / Math.PI);
				num = 8.2f;
				break;
			case 13:
				SemiMajorAxis = 0.000467431752421129;
				MeanDailyMotion = 11.2475603823412;
				e = 0.00011;
				Gamma = 0.004865279822859393;
				L = (289.50394 + 644.630418 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (153.3233 + 0.455889 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (12.84674 - 0.455584 * num2) % 360.0 / (180.0 / Math.PI);
				num = 9.6f;
				break;
			case 14:
				SemiMajorAxis = 0.000503052700529408;
				MeanDailyMotion = 10.074312583672;
				e = 7E-05;
				Gamma = 0.0005345943498858631;
				L = (318.96757 + 577.360289 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (321.74359 + 0.352762 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (279.3372 - 0.352548 * num2) % 360.0 / (180.0 / Math.PI);
				num = 9.2f;
				break;
			case 15:
				SemiMajorAxis = 0.0005749041976673585;
				MeanDailyMotion = 8.24598289468198;
				e = 0.00012;
				Gamma = 0.0055712655052910995;
				L = (331.6236 + 472.544588 * num2) % 360.0 / (180.0 / Math.PI);
				CurlyPi = (85.82748 + 0.221675 * num2) % 360.0 / (180.0 / Math.PI);
				Theta = (268.73361 - 0.221582 * num2) % 360.0 / (180.0 / Math.PI);
				num = 7.5f;
				break;
			}
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			UranusXYZRotation(out var x, out var y, out var z, MainSat: false);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 7, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)((double)num + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void UranusXYZRotation(out double x2000, out double y2000, out double z2000, bool MainSat)
		{
			double num = Math.Sin(Gamma);
			double num2 = Math.Cos(Gamma);
			double num3 = Math.Sin(Theta);
			double num4 = Math.Cos(Theta);
			double num5 = RadiusVector * Math.Cos(L - Theta);
			double num6 = RadiusVector * Math.Sin(L - Theta);
			double num7 = 0.0;
			double num8 = num5;
			double num9 = num2 * num6 - num * num7;
			double num10 = num * num6 + num2 * num7;
			double num11 = num4 * num8 - num3 * num9;
			double num12 = num3 * num8 + num4 * num9;
			double num13 = num10;
			double num17;
			double num18;
			double num19;
			if (MainSat)
			{
				double num14 = num11;
				double num15 = 0.25936185067 * num12 - 0.96578021848 * num13;
				double num16 = 0.96578021848 * num12 + 0.25936185067 * num13;
				num17 = -0.97280297147 * num14 - 0.23163414835 * num15;
				num18 = 0.23163414835 * num14 + -0.97280297147 * num15;
				num19 = num16;
			}
			else
			{
				double num14 = num11;
				double num15 = 0.2617714547 * num12 - 0.9651298904 * num13;
				double num20 = 0.9651298904 * num12 + 0.2617714547 * num13;
				num17 = -0.9755777685 * num14 - 0.2196543138 * num15;
				num18 = 0.2196543138 * num14 + -0.9755777685 * num15;
				num19 = num20;
			}
			x2000 = num17;
			y2000 = num18;
			z2000 = num19;
			if (MainSat)
			{
				Utilities.ConvertXYZ_1950to2000(ref x2000, ref y2000, ref z2000);
			}
			double num21 = Math.Sin(Gamma) * Math.Sin(Theta);
			double num22 = Math.Cos(Gamma) * 0.9651298904 + Math.Sin(Gamma) * 0.2617714547 * Math.Cos(Theta);
			double num23 = Math.Cos(Gamma) * 0.2617714547 - Math.Sin(Gamma) * 0.9651298904 * Math.Cos(Theta);
			double num24 = Math.Sin(Gamma) * 0.2617714547 + Math.Cos(Gamma) * 0.9651298904 * Math.Cos(Theta);
			double num25 = 0.9651298904 * Math.Sin(Theta);
			N = Math.Atan2(num21, num22) + 2.920132537193213;
			J = Math.Atan2(Math.Sqrt(num21 * num21 + num22 * num22), num23);
			double num26 = Math.Atan2(num25, num24);
			for (U = L - Theta + num26; U > Math.PI * 2.0; U -= Math.PI * 2.0)
			{
			}
			while (U < 0.0)
			{
				U += Math.PI * 2.0;
			}
		}

		private static bool AccurateUranusItoV(double JD, int Moon, out double x2000, out double y2000, out double z2000)
		{
			double[] array = new double[300];
			double num = JD - 2453736.5;
			int num2 = (int)(num / 29.99875);
			x2000 = (y2000 = (z2000 = 0.0));
			if (num < 0.0 || num > 2189.90875)
			{
				return false;
			}
			double num3 = num / 29.99875 % 1.0;
			double num4 = 2.0 * num3 - 1.0;
			double num5 = 2.0 * num4;
			array[0] = 1.0;
			array[1] = num4;
			for (int i = 2; i < 300; i++)
			{
				array[i] = num5 * array[i - 1] - array[i - 2];
			}
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\U" + Convert.ToString(Moon) + ".bin", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(7216L * (long)num2 + 16, SeekOrigin.Begin);
			double num6 = 0.0;
			for (int j = 0; j < 300; j++)
			{
				num6 = binaryReader.ReadDouble();
				x2000 += num6 * array[j];
				if (j == 0)
				{
					x2000 -= 0.5 * num6;
				}
			}
			for (int k = 0; k < 300; k++)
			{
				num6 = binaryReader.ReadDouble();
				y2000 += num6 * array[k];
				if (k == 0)
				{
					y2000 -= 0.5 * num6;
				}
			}
			for (int l = 0; l < 300; l++)
			{
				num6 = binaryReader.ReadDouble();
				z2000 += num6 * array[l];
				if (l == 0)
				{
					z2000 -= 0.5 * num6;
				}
			}
			x2000 /= 149597871.0;
			y2000 /= 149597871.0;
			z2000 /= 149597871.0;
			return true;
		}

		private static void Triton(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			Utilities.QuickPlanet(jd, 8, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			double num = jd - 2447763.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			e = 1.559E-05;
			Gamma = 2.7372779170268564;
			MeanDailyMotion = 61.2572637;
			SemiMajorAxis = 0.00237141954;
			L = (76.7243689 + MeanDailyMotion * num) % 360.0 / (180.0 / Math.PI);
			Theta = (172.096852 + 0.523159764 * num2) / (180.0 / Math.PI);
			CurlyPi = (171.94892 + 0.38246354 * num2) / (180.0 / Math.PI);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			Na = 0.505225589;
			Ja = 0.8147388934;
			MeanDailyMotion /= 180.0 / Math.PI;
			NeptuneXYZRotation(1, out var _, out var _, out var _);
			num = jd - 2378520.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num3 = (258.727508 + 0.00598084154 * (jd - 2451545.0)) / (180.0 / Math.PI);
			double num4 = 31.179424 - 9.8E-06 * (num / 365.25) * (num / 365.25);
			double num5 = 1.2588641713746322;
			double num6 = (73.395781 + 0.001452458 * num) / (180.0 / Math.PI);
			double num7 = 3.5044148575403207 - num6;
			double num8 = 0.00096486 * Math.Cos(2.0 * num3 + num7) + 0.00664662 * Math.Cos(num7) + 4.687E-05 * Math.Cos(-2.0 * num3 + num7) + 0.00095975 * Math.Cos(2.0 * num3 + 2.0 * num7) - 0.00037627 * Math.Cos(2.0 * num7) - 2.25E-06 * Math.Cos(-2.0 * num3 + 2.0 * num7);
			double num9 = -0.00012327 * Math.Sin(2.0 * num3) - 0.00279453 * Math.Sin(2.0 * num3 + num7) - 0.04335625 * Math.Sin(num7) - 0.00017215 * Math.Sin(-2.0 * num3 + num7) - 0.00233686 * Math.Sin(2.0 * num3 + 2.0 * num7) + 0.00170605 * Math.Sin(2.0 * num7) + 7.3E-06 * Math.Sin(-2.0 * num3 + 2.0 * num7);
			double num10 = 0.00063339 * Math.Sin(2.0 * num3) - 0.00178908 * Math.Sin(2.0 * num3 + num7) - 0.0156011 * Math.Sin(num7) - 9.186E-05 * Math.Sin(-2.0 * num3 + num7) - 0.00218071 * Math.Sin(2.0 * num3 + 2.0 * num7) + 0.00096231 * Math.Sin(2.0 * num7) + 5.36E-06 * Math.Sin(-2.0 * num3 + 2.0 * num7);
			double num11 = (num4 + 61.25873359 * num + num9) % 360.0 / (180.0 / Math.PI);
			double num12 = (157.268439 + num8) % 360.0 / (180.0 / Math.PI);
			double num13 = num5 + 2.5455522420582176E-05 * num + num10 / (180.0 / Math.PI);
			double num14 = 0.002371001389451592 * (Math.Cos(num11) * Math.Cos(num13) - Math.Sin(num11) * Math.Sin(num13) * Math.Cos(num12));
			double num15 = 0.002371001389451592 * (Math.Cos(num11) * Math.Sin(num13) + Math.Sin(num11) * Math.Cos(num13) * Math.Cos(num12));
			double num16 = 0.002371001389451592 * Math.Sin(num11) * Math.Sin(num12);
			double num17 = (0.0 - Math.Sin(5.218831169435885)) * num14 - Math.Cos(5.218831169435885) * Math.Sin(0.7492698478811657) * num15 + Math.Cos(5.218831169435885) * Math.Cos(0.7492698478811657) * num16;
			double num18 = Math.Cos(5.218831169435885) * num14 - Math.Sin(5.218831169435885) * Math.Sin(0.7492698478811657) * num15 + Math.Sin(5.218831169435885) * Math.Cos(0.7492698478811657) * num16;
			double num19 = Math.Cos(0.7492698478811657) * num15 + Math.Sin(0.7492698478811657) * num16;
			Satellites.x = num17;
			Satellites.y = num18;
			Satellites.z = num19;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 8, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(-1.24 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Nereid(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 8, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2433680.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 36525.0;
			e = 0.751201525;
			Gamma = 0.12622970228284655;
			MeanDailyMotion = 0.99962408;
			SemiMajorAxis = 0.0368549086;
			L = (254.150289 + MeanDailyMotion * num) % 360.0 / (180.0 / Math.PI);
			Theta = (333.979128 - 3.91660963 * num2) / (180.0 / Math.PI);
			CurlyPi = (254.809177 + 0.706322818 * num2) / (180.0 / Math.PI);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			Na = 0.003560192;
			Ja = 0.3638247699;
			NeptuneXYZRotation(2, out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 8, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(4.0 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void NeptuneXYZRotation(int MoonNumber, out double x2000, out double y2000, out double z2000)
		{
			double num = Math.Sin(Gamma);
			double num2 = Math.Cos(Gamma);
			double num3 = Math.Sin(Theta);
			double num4 = Math.Cos(Theta);
			double num5 = Math.Sin(Ja);
			double num6 = Math.Cos(Ja);
			double num7 = Math.Sin(Na);
			double num8 = Math.Cos(Na);
			double num9;
			double num10;
			double num11;
			if (MoonNumber == 1)
			{
				num9 = RadiusVector * Math.Cos(L + Theta);
				num10 = RadiusVector * Math.Sin(L + Theta);
				num11 = 0.0;
			}
			else
			{
				num9 = RadiusVector * Math.Cos(L - Theta);
				num10 = RadiusVector * Math.Sin(L - Theta);
				num11 = 0.0;
			}
			double num12 = num9;
			double num13 = num2 * num10 - num * num11;
			double num14 = num * num10 + num2 * num11;
			double num15 = num4 * num12 - num3 * num13;
			double num16 = num3 * num12 + num4 * num13;
			double num17 = num14;
			U = Math.Atan2(num16, num15);
			if (MoonNumber == 1)
			{
				double num18 = num15;
				double num19 = num6 * num16 - num5 * num17;
				double num20 = num5 * num16 + num6 * num17;
				x2000 = num8 * num18 - num7 * num19;
				y2000 = num7 * num18 + num8 * num19;
				z2000 = num20;
			}
			else
			{
				double num18 = num15;
				double num19 = num6 * num16 - num5 * num17;
				double num20 = num5 * num16 + num6 * num17;
				x2000 = num8 * num18 - num7 * num19;
				y2000 = num7 * num18 + num8 * num19;
				z2000 = num20;
			}
			Utilities.ConvertXYZ_1950to2000(ref x2000, ref y2000, ref z2000);
			double RA = Na - Math.PI / 2.0;
			double Dec = Math.PI / 2.0 - Ja;
			Utilities.Precession(1950, 2451545.0, use2006values_Not1976: false, ref RA, ref Dec, 0.0, 0.0);
			RA += Math.PI / 2.0;
			Dec = Math.PI / 2.0 - Dec;
			double num21 = Math.Sin(Dec);
			double num22 = Math.Cos(Dec);
			double num23 = num * num3;
			double num24 = num2 * num21 + num * num22 * num4;
			double num25 = num2 * num22 - num * num21 * num4;
			double num26 = num * num22 + num2 * num21 * num4;
			double num27 = num21 * num3;
			N = Math.Atan2(num23, num24) + RA;
			J = Math.Atan2(Math.Sqrt(num23 * num23 + num24 * num24), num25);
			double num28 = Math.Atan2(num27, num26);
			if (MoonNumber == 1)
			{
				U = L + Theta + num28;
			}
			else
			{
				U = L - Theta + num28;
			}
			while (U > Math.PI * 2.0)
			{
				U -= Math.PI * 2.0;
			}
			while (U < 0.0)
			{
				U += Math.PI * 2.0;
			}
		}

		private static void Charon(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 9, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2451544.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			MeanDailyMotion = 56.3625004315254;
			SemiMajorAxis = 0.0001309911689852859;
			e = 5E-05;
			CurlyPi = 3.3143802495372316;
			J = 1.6795303391941434;
			N = 3.892607830722953;
			L = (276.0 + MeanDailyMotion * num) % 360.0 / (180.0 / Math.PI);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			PlutoXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 9, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(0.9 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Hydra(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 9, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2451544.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			MeanDailyMotion = 9.423565720209098;
			SemiMajorAxis = 0.00043263316227274383;
			e = 0.00554;
			CurlyPi = (258.0 + 25.697 * num2) % 360.0 / (180.0 / Math.PI);
			double num3 = (122.7 - 25.798 * num2) / (180.0 / Math.PI);
			double num4 = Math.PI / 600.0;
			double num5 = 1.6795303391941434;
			L = (228.4 + MeanDailyMotion * num) % 360.0 / (180.0 / Math.PI);
			double num6 = Math.Sin(num4) * Math.Sin(num3);
			double num7 = Math.Cos(num4) * Math.Sin(num5) + Math.Sin(num4) * Math.Cos(num5) * Math.Cos(num3);
			double num8 = (0.0 - Math.Cos(num4)) * Math.Cos(num5) + Math.Sin(num4) * Math.Sin(num5) * Math.Cos(num3);
			double num9 = Math.Sin(num4) * Math.Cos(num5) + Math.Cos(num4) * Math.Sin(num5) * Math.Cos(num3);
			double num10 = Math.Sin(num5) * Math.Sin(num3);
			J = Math.PI - Math.Atan2(Math.Sqrt(num6 * num6 + num7 * num7), num8);
			N = 3.892607830722953 + Math.Atan2(num6, num7);
			double num11 = Math.PI * -2.0 + Math.Atan2(num10, num9);
			L = L - num3 + num11;
			CurlyPi = CurlyPi - num3 + num11;
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			PlutoXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 9, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(8.0 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Nix(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 9, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2451544.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			MeanDailyMotion = 14.484123790978;
			SemiMajorAxis = 0.0003254725463305557;
			e = 0.0;
			CurlyPi = 0.0;
			J = 1.6795303391941434;
			N = 3.892607830722953;
			L = (85.0 + MeanDailyMotion * num) % 360.0 / (180.0 / Math.PI);
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			PlutoXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 9, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(8.5 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Kerberos(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 9, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2451544.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			double num2 = num / 365.25;
			MeanDailyMotion = 11.191280748821029;
			SemiMajorAxis = 0.0003860349055368575;
			e = 0.0;
			CurlyPi = 0.0;
			double num3 = Math.PI / 450.0;
			double num4 = (313.3 - 39.7988 * num2) % 360.0 / (180.0 / Math.PI);
			L = (261.9 + MeanDailyMotion * num) % 360.0 / (180.0 / Math.PI);
			double num5 = 1.6795303391941434;
			double num6 = Math.Sin(num3) * Math.Sin(num4);
			double num7 = Math.Cos(num3) * Math.Sin(num5) + Math.Sin(num3) * Math.Cos(num5) * Math.Cos(num4);
			double num8 = (0.0 - Math.Cos(num3)) * Math.Cos(num5) + Math.Sin(num3) * Math.Sin(num5) * Math.Cos(num4);
			double num9 = Math.Sin(num3) * Math.Cos(num5) + Math.Cos(num3) * Math.Sin(num5) * Math.Cos(num4);
			double num10 = Math.Sin(num5) * Math.Sin(num4);
			J = Math.PI - Math.Atan2(Math.Sqrt(num6 * num6 + num7 * num7), num8);
			N = 3.892607830722953 + Math.Atan2(num6, num7);
			double num11 = Math.PI * -2.0 + Math.Atan2(num10, num9);
			L = L - num4 + num11;
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			PlutoXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 9, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(11.0 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void Styx(double jd, bool HighAccuracy, bool XYZFlag, ref double dRA, ref double dDec, ref double Sepn, ref double PA, ref float Mag)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double Distance = 0.0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double PlanetHeliocentricDistance = 0.0;
			double EquationOfCentre = 0.0;
			double UnitaryRadiusVector = 0.0;
			Utilities.QuickPlanet(jd, 9, EquinoxOfDate: false, out RA, out Dec, out Distance, out Longitude, out Latitude, out PlanetHeliocentricDistance);
			double num = jd - 2451545.5 - Convert.ToDouble(!XYZFlag) * 0.0057756 * Distance;
			MeanDailyMotion = 17.855583150698;
			SemiMajorAxis = 0.0002835133930482206;
			e = 1E-05;
			CurlyPi = Math.PI * 89.0 / 900.0;
			J = 1.6795303391941434;
			N = 3.892607830722953;
			L = (180.5 + MeanDailyMotion * num) % 360.0 / (180.0 / Math.PI);
			L += CurlyPi;
			TrueAnomaly_Radius(L - CurlyPi, e, ref EquationOfCentre, ref UnitaryRadiusVector);
			L += EquationOfCentre;
			RadiusVector = SemiMajorAxis * UnitaryRadiusVector;
			PlutoXYZRotation(out var x, out var y, out var z);
			Satellites.x = x;
			Satellites.y = y;
			Satellites.z = z;
			if (!XYZFlag)
			{
				PlanetPAandSep(jd, 9, Satellites.x, Satellites.y, Satellites.z, ref dRA, ref dDec, ref Sepn, ref PA);
			}
			else
			{
				dRA = (dDec = (dDist = (Sepn = (PA = 0.0))));
			}
			Mag = (float)(11.9 + 5.0 * Math.Log10(PlanetHeliocentricDistance * Distance));
		}

		private static void PlutoXYZRotation(out double x2000, out double y2000, out double z2000)
		{
			double num = Math.Sin(J);
			double num2 = Math.Cos(J);
			double num3 = Math.Sin(N);
			double num4 = Math.Cos(N);
			double num5 = RadiusVector * Math.Cos(L);
			double num6 = RadiusVector * Math.Sin(L);
			double num7 = 0.0;
			U = L;
			double num8 = num5;
			double num9 = num2 * num6 - num * num7;
			double num10 = num * num6 + num2 * num7;
			x2000 = num4 * num8 - num3 * num9;
			y2000 = num3 * num8 + num4 * num9;
			z2000 = num10;
		}

		public static void RelativeMoonPositions(double JD, int PlanetNumber, int Moon1, int Moon2, bool ViewedFromSun, out string EventName, out int MoonInFront, out int MoonAtRear, out double MoonAtRearRadius, out double MoonInFrontRadius, out double PenumbralRadius, out double UmbralRadius, out double dx, out double dy, out double dz, out double Sepn, out double PlanetSepn, out double PlanetPA, out double DistanceToPlanet, out float Moon1Mag, out float Moon2Mag)
		{
			double dRA = 0.0;
			double dRA2 = 0.0;
			double dDec = 0.0;
			double dDec2 = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			float MoonDiaKm = 0f;
			float MoonDiaKm2 = 0f;
			int num3 = 0;
			string[] array = new string[2];
			string[] array2 = new string[2] { " occ ", " ecl " };
			StringBuilder stringBuilder = new StringBuilder(40);
			Moon1Mag = (Moon2Mag = 22f);
			SatelliteCoordinates(JD, PlanetNumber, Moon1, ViewedFromSun, ref array[0], ref MoonDiaKm, ref dRA, ref dDec, ref Moon1Mag);
			num = dDist;
			SatelliteCoordinates(JD, PlanetNumber, Moon2, ViewedFromSun, ref array[1], ref MoonDiaKm2, ref dRA2, ref dDec2, ref Moon2Mag);
			num2 = dDist;
			DistanceToPlanet = PlanetDistance;
			int num4;
			if (num2 > num)
			{
				MoonInFront = Moon2;
				MoonAtRear = Moon1;
				MoonAtRearRadius = (double)MoonDiaKm * 0.0013787950648515336 / DistanceToPlanet / 2.0;
				MoonInFrontRadius = (double)MoonDiaKm2 * 0.0013787950648515336 / DistanceToPlanet / 2.0;
				dx = (dRA2 - dRA) * (648000.0 / Math.PI);
				dy = (dDec2 - dDec) * (648000.0 / Math.PI);
				dz = num2 - num;
				num4 = 1;
			}
			else
			{
				MoonInFront = Moon1;
				MoonAtRear = Moon2;
				MoonAtRearRadius = (double)MoonDiaKm2 * 0.0013787950648515336 / DistanceToPlanet / 2.0;
				MoonInFrontRadius = (double)MoonDiaKm * 0.0013787950648515336 / DistanceToPlanet / 2.0;
				dx = (dRA - dRA2) * (648000.0 / Math.PI);
				dy = (dDec - dDec2) * (648000.0 / Math.PI);
				dz = num - num2;
				num4 = 0;
			}
			if (!ViewedFromSun)
			{
				UmbralRadius = MoonInFrontRadius;
				PenumbralRadius = MoonInFrontRadius;
				num3 = 0;
			}
			else
			{
				double num5 = 959.63 * dz / Math.Pow(DistanceToPlanet, 2.0);
				UmbralRadius = MoonInFrontRadius - num5;
				PenumbralRadius = MoonInFrontRadius + num5;
				num3 = 1;
			}
			if (num4 == 1)
			{
				stringBuilder.Append(array[1].Substring(array[1].IndexOf("(")));
				stringBuilder.Append(array2[num3]);
				stringBuilder.Append(array[0].Substring(array[0].IndexOf("(")));
			}
			else
			{
				stringBuilder.Append(array[0].Substring(array[0].IndexOf("(")));
				stringBuilder.Append(array2[num3]);
				stringBuilder.Append(array[1].Substring(array[1].IndexOf("(")));
			}
			EventName = stringBuilder.ToString();
			Sepn = (PlanetSepn = (PlanetPA = 0.0));
		}

		public static void MutualEventTimes(double JD, double Hour, int PlanetNumber, int Moon1, int Moon2, bool ViewedFromSun, ref double MissDistance_arcsec, out string Events, out string EventTimes)
		{
			double[] array = new double[7];
			double Sepn = 0.0;
			double PA = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 1.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double day = 0.0;
			int Year = 0;
			int Month = 0;
			int num8 = 0;
			bool flag = true;
			bool flag2 = true;
			bool flag3 = true;
			bool flag4 = true;
			bool flag5 = false;
			bool flag6 = false;
			StringBuilder stringBuilder = new StringBuilder(80);
			StringBuilder stringBuilder2 = new StringBuilder(80);
			if (MissDistance_arcsec < 0.0)
			{
				MissDistance_arcsec = 0.0;
			}
			double num9 = Hour - 0.05;
			Utilities.Date_from_JD(JD, out Year, out Month, out day);
			int num10 = 0;
			string EventName;
			int MoonInFront;
			int MoonAtRear;
			double MoonAtRearRadius;
			double MoonInFrontRadius;
			double PenumbralRadius;
			double UmbralRadius;
			double dz;
			double Sepn2;
			double PlanetSepn;
			double PlanetPA;
			double DistanceToPlanet;
			float Moon1Mag;
			float Moon2Mag;
			double num11;
			double num12;
			double num15;
			double num16;
			double num13;
			double num14;
			double num18;
			double num19;
			do
			{
				RelativeMoonPositions(JD + num9 / 24.0, PlanetNumber, Moon1, Moon2, ViewedFromSun, out EventName, out MoonInFront, out MoonAtRear, out MoonAtRearRadius, out MoonInFrontRadius, out PenumbralRadius, out UmbralRadius, out var dx, out var dy, out dz, out Sepn2, out PlanetSepn, out PlanetPA, out DistanceToPlanet, out Moon1Mag, out Moon2Mag);
				num11 = dx;
				num12 = dy;
				num13 = PenumbralRadius;
				num14 = UmbralRadius;
				RelativeMoonPositions(JD + (num9 + 0.1) / 24.0, PlanetNumber, Moon1, Moon2, ViewedFromSun, out EventName, out MoonInFront, out MoonAtRear, out MoonAtRearRadius, out MoonInFrontRadius, out PenumbralRadius, out UmbralRadius, out dx, out dy, out dz, out Sepn2, out PlanetSepn, out PlanetPA, out DistanceToPlanet, out Moon1Mag, out Moon2Mag);
				num15 = dx - num11;
				num16 = dy - num12;
				num13 = (num13 + PenumbralRadius) / 2.0;
				num14 = (num14 + UmbralRadius) / 2.0;
				double num17 = num11 * num15 + num12 * num16;
				num18 = num15 * num15 + num16 * num16;
				num19 = (0.0 - num17) / num18 * 0.1 - 0.05;
				num9 += num19;
				num10++;
			}
			while (Math.Abs(num19) > 0.02 && num10 < 8);
			double num23;
			if (num10 > 7)
			{
				for (double num20 = 0.0; num20 <= 2.0; num20 += 1.0)
				{
					RelativeMoonPositions(JD + (num9 - 0.1) / 24.0, PlanetNumber, Moon1, Moon2, ViewedFromSun, out EventName, out MoonInFront, out MoonAtRear, out MoonAtRearRadius, out MoonInFrontRadius, out PenumbralRadius, out UmbralRadius, out var dx2, out var dy2, out dz, out Sepn2, out PlanetSepn, out PlanetPA, out DistanceToPlanet, out Moon1Mag, out Moon2Mag);
					double num21 = Math.Sqrt(dx2 * dx2 + dy2 * dy2);
					RelativeMoonPositions(JD + num9 / 24.0, PlanetNumber, Moon1, Moon2, ViewedFromSun, out EventName, out MoonInFront, out MoonAtRear, out MoonAtRearRadius, out MoonInFrontRadius, out PenumbralRadius, out UmbralRadius, out var dx3, out var dy3, out dz, out Sepn2, out PlanetSepn, out PlanetPA, out DistanceToPlanet, out Moon1Mag, out Moon2Mag);
					num = Math.Sqrt(dx3 * dx3 + dy3 * dy3);
					RelativeMoonPositions(JD + (num9 + 0.1) / 24.0, PlanetNumber, Moon1, Moon2, ViewedFromSun, out EventName, out MoonInFront, out MoonAtRear, out MoonAtRearRadius, out MoonInFrontRadius, out PenumbralRadius, out UmbralRadius, out var dx4, out var dy4, out dz, out Sepn2, out PlanetSepn, out PlanetPA, out DistanceToPlanet, out Moon1Mag, out Moon2Mag);
					double num22 = Math.Sqrt(dx4 * dx4 + dy4 * dy4);
					num5 = (num21 + num22 - 2.0 * num) / 2.0;
					num6 = num22 - num - num5;
					num7 = (0.0 - num6) / 2.0 / num5 * 0.1;
					num9 += num7;
				}
				Hour = num9 - num7;
				num23 = num5 * num7 * num7 + num6 * num7 + num;
			}
			else
			{
				Hour = num9 + 0.05;
				num23 = Math.Abs((num11 * num16 - num12 * num15) / Math.Sqrt(num18));
			}
			array[3] = Hour - Utilities.delta_T(Year, Month, day) / 3600.0;
			if ((array[3] < -12.0) | (array[3] > 36.0))
			{
				EventTimes = "";
				Events = "";
				return;
			}
			num8 = 0;
			if (array[3] < 0.0)
			{
				array[3] += 24.0;
				num8 = -1;
			}
			if (array[3] >= 24.0)
			{
				array[3] -= 24.0;
				num8 = 1;
			}
			Utilities.Date_from_JD(JD + (double)num8, out Year, out Month, out day);
			stringBuilder.AppendFormat("{0,4:D0}", Year);
			stringBuilder.AppendFormat("{0,3:D0}", Month);
			stringBuilder.AppendFormat("{0,3:N0}", day);
			stringBuilder.Append(Utilities.DEGtoDMS(array[3], 3, 0, MinutesOnly: false));
			if (num10 > 4)
			{
				stringBuilder.Append("* ");
			}
			else
			{
				stringBuilder.Append("  ");
			}
			stringBuilder.Append(EventName.PadRight(16).Substring(0, 16));
			bool flag7 = false;
			if (num23 > num13 + MoonAtRearRadius)
			{
				stringBuilder.Append("M");
				if (num23 > num13 + MoonAtRearRadius + MissDistance_arcsec)
				{
					EventTimes = "";
					Events = "";
					return;
				}
				flag7 = true;
			}
			else if (num23 > num14 + MoonAtRearRadius || num14 < 0.0)
			{
				stringBuilder.Append("E");
			}
			else if (num14 > MoonAtRearRadius)
			{
				if (num23 > num14 - MoonAtRearRadius)
				{
					stringBuilder.Append("P");
				}
				else
				{
					stringBuilder.Append("T");
				}
			}
			else if (num23 > MoonAtRearRadius - num14)
			{
				stringBuilder.Append("P");
			}
			else
			{
				stringBuilder.Append("A");
			}
			int num24 = 0;
			if (!flag7)
			{
				for (int i = 0; i <= 2; i++)
				{
					array[i] = 0.0;
					switch (i)
					{
					case 0:
						num4 = num13 + MoonAtRearRadius;
						break;
					case 1:
						num4 = num14 + MoonAtRearRadius;
						break;
					case 2:
						num4 = num14 - MoonAtRearRadius;
						break;
					}
					double num20 = num23 / num4;
					if (Math.Abs(num20) < 1.0)
					{
						double num25 = Math.Abs(num4) * Math.Sqrt(1.0 - num20 * num20) / Math.Sqrt(num18) * 0.1;
						array[i] = array[3] - num25;
						array[6 - i] = array[3] + num25;
						if (num25 > 4.0)
						{
							num25 = 4.0;
						}
						if (num24 == 0)
						{
							num24 = Convert.ToInt16(7200.0 * num25);
						}
					}
				}
				for (int i = 0; i <= 6; i++)
				{
					if (array[i] == 0.0 || (!ViewedFromSun && (i == 0 || i == 6)))
					{
						stringBuilder2.Append("          ");
						continue;
					}
					if (array[i] < 0.0)
					{
						array[i] += 24.0;
						stringBuilder2.Append("p");
					}
					else if (array[i] >= 24.0)
					{
						array[i] -= 24.0;
						stringBuilder2.Append("f");
					}
					else
					{
						stringBuilder2.Append(" ");
					}
					stringBuilder2.Append(Utilities.DEGtoDMS(array[i], 2, 0, MinutesOnly: false) + " ");
				}
			}
			EventTimes = stringBuilder2.ToString();
			SunToMoonRatio = 1.0 - UmbralRadius / MoonInFrontRadius;
			num3 = GetBrightness(PlanetNumber, ViewedFromSun, MoonAtRearRadius, MoonInFrontRadius, UmbralRadius, PenumbralRadius, num14, num23, MoonInFront, MoonAtRear);
			num2 = ((num3 <= 0.0002) ? 9.9 : ((!(num3 >= 1.0)) ? (-2.5 * Math.Log10(num3)) : 0.0));
			SatelliteCoordinates(JD + (double)num8 + array[3] / 24.0, PlanetNumber, MoonAtRear, ViewedFromSun: true, ref Sepn, ref PA);
			flag2 = Sepn * (648000.0 / Math.PI) > PlanetRadiusArcSec;
			flag5 = dDist < 0.0;
			SatelliteCoordinates(JD + (double)num8 + array[3] / 24.0, PlanetNumber, MoonInFront, ViewedFromSun: true, ref Sepn, ref PA);
			flag3 = Sepn * (648000.0 / Math.PI) > PlanetRadiusArcSec;
			flag6 = dDist < 0.0;
			SatelliteCoordinates(JD + (double)num8 + array[3] / 24.0, PlanetNumber, MoonAtRear, ViewedFromSun: false, ref PlanetSepn, ref PlanetPA);
			flag = PlanetSepn * (648000.0 / Math.PI) > PlanetRadiusArcSec;
			flag4 = dDist < 0.0;
			string value = " ";
			if (ViewedFromSun)
			{
				if (!flag2 || !flag)
				{
					if (flag4)
					{
						value = "h";
					}
					else if (!flag)
					{
						value = "t";
					}
				}
			}
			else if (!flag || (!flag2 && flag5) || (!flag3 && flag6))
			{
				value = ((!flag && flag4) ? "h" : ((!flag3 && flag6 && flag2) ? "e" : ((!flag2 && flag5 && flag3) ? "f" : ((!(!flag2 && flag5 && !flag3 && flag6)) ? "t" : "g"))));
			}
			if ("fgh".Contains(value) & !IncludeHiddenEvents)
			{
				EventTimes = "";
				Events = "";
				return;
			}
			stringBuilder.Append(value);
			stringBuilder.AppendFormat("{0,5:F0}", num24);
			stringBuilder.AppendFormat("{0,5:F1}", num2);
			stringBuilder.AppendFormat("{0,6:F1}", num3 * 100.0);
			stringBuilder.AppendFormat("{0,6:F1}", PlanetSepn * (648000.0 / Math.PI));
			stringBuilder.AppendFormat("{0,4:F0}", PlanetPA * (180.0 / Math.PI));
			stringBuilder.AppendFormat("{0,7:F3}", Math.Abs(num23));
			Events = stringBuilder.ToString();
		}

		internal static double GetBrightness(int PlanetNumber, bool ViewedFromSun, double MoonAtRearRadius, double MoonInFrontRadius, double UmbralRadius, double PenumbralRadius, double L2, double Delta, int MoonInFront, int MoonAtRear)
		{
			double num = 1.0;
			double num2 = 1.0;
			if (!ViewedFromSun)
			{
				num = SatelliteAlbedo(PlanetNumber, MoonAtRear) / SatelliteAlbedo(PlanetNumber, MoonInFront);
				double num3 = Math.PI * UmbralRadius * UmbralRadius + Math.PI * MoonAtRearRadius * MoonAtRearRadius * num;
				double num4 = EclipseArea(MoonAtRearRadius, UmbralRadius, Delta);
				num2 = ((Delta - MoonAtRearRadius > UmbralRadius) ? 1.0 : ((Delta + MoonAtRearRadius <= UmbralRadius) ? (Math.PI * UmbralRadius * UmbralRadius / num3) : ((!(Delta + UmbralRadius <= MoonAtRearRadius)) ? ((Math.PI * UmbralRadius * UmbralRadius + (Math.PI * MoonAtRearRadius * MoonAtRearRadius - num4) * num) / num3) : ((Math.PI * UmbralRadius * UmbralRadius + Math.PI * (MoonAtRearRadius * MoonAtRearRadius - UmbralRadius * UmbralRadius) * num) / num3))));
			}
			else if (ViewedFromSun)
			{
				if (Delta + MoonAtRearRadius <= UmbralRadius)
				{
					num2 = 0.0;
				}
				else
				{
					_ = L2 / MoonInFrontRadius;
					CalculateShadowBrightness(ref ShadowBrightness);
					double num5 = MoonAtRearRadius * MoonAtRearRadius * Math.PI;
					num2 = 1.0;
					double num6 = 0.0;
					for (double num7 = 0.0; num7 < 100.0; num7 += 1.0)
					{
						double num8 = EclipseArea(MoonAtRearRadius, PenumbralRadius * num7 / 100.0, Delta) / num5;
						num2 -= (1.0 - (ShadowBrightness[(int)num7] + ShadowBrightness[(int)num7 + 1]) / 2.0) * (num8 - num6);
						num6 = num8;
					}
				}
			}
			return num2;
		}

		internal static void CalculateShadowBrightness(ref double[] Shadow)
		{
			double[] array = new double[100]
			{
				1.23016134, 1.23010304, 1.229986407, 1.229811372, 1.229577834, 1.22928566, 1.228934679, 1.22852469, 1.228055455, 1.227526702,
				1.226938128, 1.226289391, 1.225580117, 1.224809896, 1.223978283, 1.223084796, 1.22212892, 1.221110101, 1.22002775, 1.218881239,
				1.217669905, 1.216393044, 1.215049915, 1.213639737, 1.212161688, 1.210614907, 1.208998488, 1.207311484, 1.205552904, 1.203721712,
				1.201816825, 1.199837111, 1.197781391, 1.195648434, 1.193436954, 1.191145612, 1.188773013, 1.186317698, 1.183778149, 1.181152782,
				1.178439944, 1.17563791, 1.172744877, 1.169758964, 1.166678204, 1.163500538, 1.160223812, 1.156845769, 1.153364042, 1.149776146,
				1.146079471, 1.142271269, 1.138348648, 1.134308554, 1.130147766, 1.125862873, 1.121450264, 1.116906106, 1.112226325, 1.107406582,
				1.10244225, 1.097328381, 1.092059676, 1.086630446, 1.081034571, 1.075265453, 1.069315957, 1.063178354, 1.056844243, 1.050304473,
				1.043549043, 1.036566991, 1.029346265, 1.021873565, 1.014134169, 1.006111711, 0.99778793, 0.989142359, 0.980151957, 0.97079065,
				0.961028781, 0.950832408, 0.940162446, 0.928973564, 0.917212771, 0.904817576, 0.891713555, 0.877811066, 0.86300073, 0.847147052,
				0.830079176, 0.811576992, 0.791349424, 0.768998705, 0.743957674, 0.715369915, 0.681831438, 0.64072199, 0.585787408, 0.46480901
			};
			_ = SunToMoonRatio;
			_ = SunToMoonRatio;
			double radiusOfUmbra = 1.0 / SunToMoonRatio;
			double num = array.Length;
			for (int i = 0; (double)i <= 100.0; i++)
			{
				double delta = (double)i / 100.0 * (1.0 + 1.0 / SunToMoonRatio);
				double num2 = 0.0;
				double num3 = 0.0;
				for (double num4 = 0.0; num4 < num; num4 += 1.0)
				{
					double num5 = (num4 + 1.0) / num;
					double num6 = num5 * num5 * Math.PI + 1E-11 - EclipseArea(num5, radiusOfUmbra, delta);
					double num7 = array[(int)num4] * (num6 - num2);
					num3 += num7;
					num2 = num6;
				}
				Shadow[i] = num3 / Math.PI;
			}
		}

		private static double EclipseArea(double RadiusOfMoon, double RadiusOfUmbra, double Delta)
		{
			if (RadiusOfUmbra + Delta <= RadiusOfMoon)
			{
				return RadiusOfUmbra * RadiusOfUmbra * Math.PI;
			}
			if (RadiusOfUmbra >= Delta + RadiusOfMoon)
			{
				return RadiusOfMoon * RadiusOfMoon * Math.PI;
			}
			if (RadiusOfUmbra + RadiusOfMoon <= Delta)
			{
				return 0.0;
			}
			double num = Math.Acos((RadiusOfUmbra * RadiusOfUmbra + Delta * Delta - RadiusOfMoon * RadiusOfMoon) / 2.0 / Math.Abs(RadiusOfUmbra) / Delta);
			if (double.IsNaN(num))
			{
				return 0.0;
			}
			double num2 = Math.Acos((RadiusOfMoon * RadiusOfMoon + Delta * Delta - RadiusOfUmbra * RadiusOfUmbra) / 2.0 / RadiusOfMoon / Delta);
			if (double.IsNaN(num2))
			{
				return 0.0;
			}
			return RadiusOfUmbra * RadiusOfUmbra * (num - Math.Sin(2.0 * num) / 2.0) + RadiusOfMoon * RadiusOfMoon * (num2 - Math.Sin(2.0 * num2) / 2.0);
		}

		public static void FindMutualEvents(double JD, int PlanetNumber, bool Eclipses, bool Occultations, double MissDistance, bool LocalEventsOnly, double SiteLongitude, double SiteLatitude, out int NumberOfEvents, ref string[] Events, ref string[] EventTimes)
		{
			int num = 0;
			double Sepn = 0.0;
			double PA = 0.0;
			double[] array = new double[3];
			double[] array2 = new double[2];
			double[,,] array3 = new double[10, 2, 2];
			double[,,] array4 = new double[10, 2, 2];
			double[,,] array5 = new double[10, 2, 2];
			double[,,] array6 = new double[10, 2, 2];
			double[,,] array7 = new double[10, 26, 2];
			double[,,] array8 = new double[10, 26, 2];
			double[] A = new double[21];
			double num2 = 0.0;
			bool flag = true;
			NumberOfEvents = 0;
			if (!Eclipses && !Occultations)
			{
				Eclipses = true;
				Occultations = true;
			}
			if (MissDistance < 0.0)
			{
				MissDistance = 0.0;
			}
			int num3;
			switch (PlanetNumber)
			{
			default:
				return;
			case 5:
				num3 = 4;
				break;
			case 6:
				num3 = 8;
				break;
			case 7:
				num3 = 5;
				break;
			}
			int num4 = 1 - Convert.ToInt16(Occultations);
			int num5 = Convert.ToInt16(Eclipses);
			for (int i = num4; i <= num5; i++)
			{
				for (int j = 1; j <= num3; j++)
				{
					for (int k = 0; k <= 1; k++)
					{
						SatelliteCoordinates(JD + (double)k, PlanetNumber, j, i == 1, ref Sepn, ref PA);
						array3[j, k, i] = U - UPlanet;
						array5[j, k, i] = BPlanet;
						array6[j, k, i] = PPlanet;
						array4[j, k, i] = RMoon;
					}
				}
			}
			for (int i = num4; i <= num5; i++)
			{
				for (int j = 1; j <= num3; j++)
				{
					double num6;
					for (num6 = array3[j, 1, i] - array3[j, 0, i]; num6 < 0.0; num6 += Math.PI * 2.0)
					{
					}
					if (PlanetNumber == 6 && j == 1 && num6 < Math.PI * 2.0)
					{
						num6 += Math.PI * 2.0;
					}
					num6 /= 24.0;
					double num7 = (array4[j, 0, i] + array4[j, 1, i]) / 2.0;
					double num8 = array6[j, 0, i] - array6[1, 0, i];
					for (int l = 0; l <= 25; l++)
					{
						double num9 = num7 * Math.Sin(array3[j, 0, i] + (double)l * num6);
						double num10 = num7 * Math.Cos(array3[j, 0, i] + (double)l * num6) * Math.Sin(array5[j, 0, i]);
						array7[j, l, i] = num9 * Math.Cos(num8) - num10 * Math.Sin(num8);
						array8[j, l, i] = num9 * Math.Sin(num8) + num10 * Math.Cos(num8);
					}
				}
			}
			for (int i = num4; i <= num5; i++)
			{
				for (int j = 1; j < num3; j++)
				{
					for (int m = j + 1; m <= num3; m++)
					{
						for (int l = 0; l < 24; l++)
						{
							array[0] = array7[j, l, i] - array7[m, l, i];
							array[1] = array7[j, l + 1, i] - array7[m, l + 1, i];
							array[2] = array7[j, l + 2, i] - array7[m, l + 2, i];
							if (!((Math.Sign(array[0]) != Math.Sign(array[1])) | ((Math.Abs(array[1]) < 2.2 + MissDistance) & (Math.Sign(array[1] - array[0]) != Math.Sign(array[2] - array[1])))))
							{
								continue;
							}
							double num11 = array[0] / (array[0] - array[1]);
							array2[0] = array8[j, l, i] - array8[m, l, i];
							array2[1] = array8[j, l + 1, i] - array8[m, l + 1, i];
							if (!(Math.Abs(array2[0] + num11 * (array2[1] - array2[0])) < (double)(6 - Convert.ToInt16(PlanetNumber == 6) - 2 * Convert.ToInt16(PlanetNumber == 7)) + MissDistance))
							{
								continue;
							}
							num11 += (double)l;
							MutualEventTimes(JD, num11, PlanetNumber, j, m, i == 1, ref MissDistance, out Events[NumberOfEvents], out EventTimes[NumberOfEvents]);
							flag = true;
							if (Events[NumberOfEvents] != "")
							{
								num2 = (double)int.Parse(Events[NumberOfEvents].Substring(11, 2)) + (double)int.Parse(Events[NumberOfEvents].Substring(14, 2)) / 60.0;
								num = 0;
								if (num2 - num11 > 12.0)
								{
									num = -1;
								}
								if (num2 - num11 < -12.0)
								{
									num = 1;
								}
								if (LocalEventsOnly)
								{
									Utilities.QuickPlanet(JD + (double)num + num2 / 24.0, PlanetNumber, EquinoxOfDate: true, out var RA, out var Dec, out var _);
									flag = Utilities.ObjectUp_SunBelowCivil(JD + (double)num, num2, RA, Dec, SiteLongitude, SiteLatitude, RequireSunDown: true, 5.0);
									if (flag && Events[NumberOfEvents] != "")
									{
										Events[NumberOfEvents] += string.Format("{0,4:f0}", 180.0 / Math.PI * Utilities.Altitude(JD + (double)num + num2 / 24.0, RA, Dec, SiteLongitude, SiteLatitude));
									}
								}
								else if (Events[NumberOfEvents] != "")
								{
									Events[NumberOfEvents] += "    ";
								}
							}
							if (flag && Events[NumberOfEvents] != "")
							{
								A[NumberOfEvents] = JD + (double)num + num2 / 24.0;
								NumberOfEvents++;
							}
						}
					}
				}
			}
			int num12 = NumberOfEvents - 1;
			int num13 = num12 / 2;
			if (num13 == 0)
			{
				num13 = 1;
			}
			do
			{
				int num14 = num12 - num13;
				int num15;
				do
				{
					num15 = 0;
					for (int n = 0; n <= num14; n++)
					{
						if (A[n] > A[n + num13])
						{
							Utilities.Swap(ref A, n, num13);
							Utilities.Swap(ref Events, n, num13);
							Utilities.Swap(ref EventTimes, n, num13);
							num15 = n;
						}
					}
					num14 = num15 - num13;
				}
				while (num15 > 0);
				num13 /= 2;
			}
			while (num13 > 0);
		}

		public static void MutualEvents_Show()
		{
			try
			{
				((Control)Mutual).Show();
			}
			catch
			{
				Mutual = new SatelliteMutualEvents();
				((Control)Mutual).Show();
			}
			((Control)Mutual).Focus();
		}

		public static void MutualLightCurve_Show()
		{
			try
			{
				((Control)MutualLightCurve).Show();
			}
			catch
			{
				MutualLightCurve = new MutualLightCurve();
				((Control)MutualLightCurve).Show();
			}
			((Control)MutualLightCurve).Focus();
		}

		internal static void DrawLightCurve(PictureBox Output, bool ShowLabels, double VerticalScaleAdjust)
		{
			if (LightCurve == null)
			{
				return;
			}
			Pen pen = new Pen(Color.Black);
			Pen pen2 = new Pen(Color.Black);
			int height = ((Control)Output).get_Height();
			int width = ((Control)Output).get_Width();
			Bitmap image = new Bitmap(width, height);
			height--;
			width--;
			Graphics graphics = Graphics.FromImage(image);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.Beige);
			pen.Color = Color.Black;
			graphics.DrawRectangle(pen, 0, 0, width, height);
			float num = (float)height * 0.1f;
			float num2 = height - 10 - 23 * Convert.ToInt32(ShowLabels);
			float num3 = 0f;
			float num4 = ((float)width - num3) / (float)(LightCurveCount - 1);
			float num5 = num2 - num;
			float num6 = 0f;
			float num7 = 0f;
			float x = 0f;
			float y = 0f;
			double num8 = 0.0;
			float num9 = 1f;
			string text = "NTSC";
			pen.Color = Color.DarkRed;
			pen.Width = 0.5f;
			graphics.DrawLine(pen, 0f, num2, width, num2);
			if (ShowLabels)
			{
				((Control)MutualLightCurve).set_Text(EventName);
				graphics.FillRectangle(Brushes.BurlyWood, 1f, num2 + 1f, width - 2, height - 2);
				pen.Color = Color.Black;
				pen.DashPattern = new float[2] { 1.5f, 18f };
				for (int num10 = 100; num10 >= 0; num10--)
				{
					num7 = num2 - (float)((double)(num5 * (float)num10) / 100.0);
					if (num10 % 10 == 0)
					{
						graphics.DrawLine(pen, 0f, num7, width, num7);
						graphics.DrawLine(pen2, 0f, num7, 8f, num7);
						graphics.DrawLine(pen2, width, num7, width - 10, num7);
						graphics.DrawString(string.Format("{0,2:f1}", (double)num10 / 100.0), new Font("Courier New", 10f), Brushes.Black, 10f, num7 - 7f);
					}
					else if (num10 % 5 == 0)
					{
						graphics.DrawLine(pen2, 0f, num7, 6f, num7);
						graphics.DrawLine(pen2, width, num7, width - 6, num7);
					}
					else
					{
						graphics.DrawLine(pen2, 0f, num7, 3f, num7);
						graphics.DrawLine(pen2, width, num7, width - 3, num7);
					}
				}
				pen.Color = Color.FromArgb(80, 100, 100, 100);
				pen.Width = 0.5f;
				pen.DashStyle = DashStyle.Solid;
				if (DisplayInMinutes)
				{
					for (int i = 0; i < LightCurveCount; i++)
					{
						num8 = LightCurveStepInterval / 60.0 * (double)i;
						if ((num8 % 1.0 < 0.01) | (num8 % 1.0 > 0.99))
						{
							graphics.DrawLine(pen2, num3 + (float)i * num4, num2, num3 + (float)i * num4, num2 - 4f);
						}
						if (i % 12 == 0)
						{
							num6 = num3 + (float)i * num4;
							if (i > 0)
							{
								graphics.DrawLine(pen, num6, 0f, num6, num2);
							}
							string text2 = Utilities.DateTime_from_JD(LightCurveStartTime_UT + num8 / 1440.0);
							string text3 = text2.Substring(text2.Length - 3);
							graphics.DrawString(text3.Trim(), new Font("Courier New", 10f), Brushes.Black, num6 - (float)(14 * Convert.ToInt32(i != 0)), num2 + 2f);
						}
						graphics.DrawString("UTC [minutes]", new Font("Courier New", 10f), Brushes.Black, width / 2 - 40, num2 + 15f);
					}
				}
				else
				{
					int num11;
					if (DisplayInNTSC)
					{
						num11 = (int)((LightCurveEndTime_UT - LightCurveStartTime_UT) * 86400.0 * 29.97);
						text = "NTSC";
					}
					else
					{
						num11 = (int)((LightCurveEndTime_UT - LightCurveStartTime_UT) * 86400.0 * 25.0);
						text = "PAL";
					}
					num9 = (float)width / (float)num11;
					int num12 = 1000 * (num11 / 10000);
					for (int j = 0; j < num11; j += num12)
					{
						num6 = num3 + (float)j * num9;
						if (j > 0)
						{
							graphics.DrawLine(pen, num6, 0f, num6, num2);
						}
						string text4 = j.ToString();
						graphics.DrawString(text4.Trim(), new Font("Arial", 7f), Brushes.Black, num6 - (float)(14 * Convert.ToInt32(j != 0)), num2 + 2f);
					}
					graphics.DrawString("Frame number " + text, new Font("Courier New", 10f, FontStyle.Bold), Brushes.Black, width / 2 - 60, num2 + 15f);
				}
				pen.Color = Color.Black;
				pen.DashPattern = new float[2] { 3f, 5f };
				graphics.DrawLine(pen, 0f, (float)((double)num2 + (double)num5 * (VerticalScaleAdjust - 1.0)), width, (float)((double)num2 + (double)num5 * (VerticalScaleAdjust - 1.0)));
				if ((double)num2 + (double)num5 * (VerticalScaleAdjust - 1.0) > (double)(height - 2))
				{
					pen.Color = Color.Red;
					pen.Width = 2f;
					graphics.DrawLine(pen, 0f, height - 1, width, height - 1);
				}
				graphics.DrawString(EventName, new Font("Arial", 10f), Brushes.Black, 10f, 10f);
			}
			pen.Color = Color.Black;
			pen.Width = 1.7f;
			pen.DashStyle = DashStyle.Solid;
			for (int k = 0; k <= LightCurveCount; k++)
			{
				num6 = num3 + (float)k * num4;
				num7 = num2 - (float)((double)num5 * ScaledLightCurve(k, VerticalScaleAdjust));
				if (k > 0)
				{
					graphics.DrawLine(pen, x, y, num6, num7);
				}
				x = num6;
				y = num7;
			}
			Output.set_Image((Image)image);
			graphics.Dispose();
		}

		private static double ScaledLightCurve(int Posn, double Scale)
		{
			if (Scale == 1.0)
			{
				return LightCurve[Posn];
			}
			return 1.0 - (1.0 - LightCurve[Posn]) * Scale;
		}

		public static bool SatellitePlanetEclipsesTransits(double JD, int Planet, int Moon, bool Eclipse, bool LocalEventsOnly, double SiteLongitude, double SiteLatitude, ref double[] EventTimes, ref int[] EventTypes, out int EventCount)
		{
			double Sepn = 0.0;
			double PA = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double[] array = new double[3];
			double[] array2 = new double[3];
			double[] array3 = new double[3];
			double[] array4 = new double[5] { 1.239, 1.526, 1.95, 2.03, 2.269 };
			double[] array5 = new double[3];
			double[] array6 = new double[3];
			double[] array7 = new double[3];
			_ = new double[4];
			bool result = false;
			bool flag = false;
			bool[] array8 = new bool[15];
			int num4 = 0;
			int num5 = 0;
			int num6 = 4;
			_ = new int[4];
			EventCount = 0;
			if (Planet < 5 || Planet > 8)
			{
				return result;
			}
			if (Planet == 5 && Moon > 4)
			{
				return result;
			}
			if (Planet == 6 && (Moon < 3 || Moon > 6) && Moon != 8)
			{
				return result;
			}
			if (Planet == 7 && Moon > 5)
			{
				return result;
			}
			if (Planet == 8 && Moon > 1)
			{
				return result;
			}
			EventCount = 0;
			for (int i = 0; i < 24; i++)
			{
				EventTypes[i] = 0;
			}
			Utilities.PlanetGeocentric(JD + 0.5, Planet, 0.0, 0, physicalFlag: true, out var _, out var _, out var _, out var RA, out var Dec, out var _, out var _, out var _, out var _, out var _, out var PhaseAngle_deg, out var _, out var PAlimb_deg, out var Planetocentric_Latitude_deg, out var PAPole_deg, out var _);
			double num7 = Math.Cos(PhaseAngle_deg / (180.0 / Math.PI)) - 1.0;
			double num8 = (90.0 - Math.Abs(Planetocentric_Latitude_deg)) / (180.0 / Math.PI);
			switch (Planet)
			{
			case 5:
			{
				double num9 = 0.96892 + 0.03249 * Math.Cos(2.0 * num8) - 0.00136 * Math.Cos(4.0 * num8);
				num3 = 1.0 - num9;
				break;
			}
			case 6:
			{
				double num9 = 0.954165 + 0.04879 * Math.Cos(2.0 * num8) - 0.003136 * Math.Cos(4.0 * num8);
				num3 = 1.0 - num9;
				break;
			}
			case 7:
			{
				double num9 = 0.988537 + 0.011463 * Math.Cos(2.0 * num8);
				num3 = 0.022927;
				break;
			}
			case 8:
			{
				double num9 = 0.9914 + 0.0086 * Math.Cos(2.0 * num8);
				num3 = 0.0171;
				break;
			}
			}
			double num10 = 1.0 - num3 / 2.0 + 0.3125 * num3 * num3 + 5.0 / 32.0 * num3 * num3 * num3;
			double num11 = num3 / 2.0 - 13.0 / 64.0 * num3 * num3 * num3;
			double num12 = -5.0 * (num3 * num3 / 16.0 + num3 * num3 * num3 / 32.0);
			double num13 = Utilities.delta_T(JD) / 86400.0;
			num4 = 0;
			num6 = 4;
			for (int j = 0; j <= 1; j++)
			{
				SatelliteCoordinates(JD, Planet, Moon, j == 1, ref Sepn, ref PA);
				num = U - UPlanet;
				double meanDailyMotion = MeanDailyMotion;
				if (Eclipse)
				{
					num += Math.PI;
				}
				while (num > Math.PI)
				{
					num -= Math.PI * 2.0;
				}
				for (; num < -Math.PI; num += Math.PI * 2.0)
				{
				}
				double num14 = (0.0 - num) / meanDailyMotion * 24.0;
				double num15 = 1.5;
				if (Moon > 2)
				{
					num15 = 2.0;
				}
				if (Moon == 8)
				{
					num15 = 6.0;
				}
				for (int k = 0; k <= 2; k++)
				{
					SatelliteCoordinates(JD + (num14 + (double)(k - 1) * num15) / 24.0, Planet, Moon, j == 1, ref Sepn, ref PA);
					array[k] = Sepn * Math.Sin(PA - PAPole_deg / (180.0 / Math.PI)) * (180.0 / Math.PI) * 3600.0;
					array2[k] = Sepn * Math.Cos(PA - PAPole_deg / (180.0 / Math.PI)) * (180.0 / Math.PI) * 3600.0;
					if (Moon == 8)
					{
						array3[k] = 83.183 / PlanetDistance;
					}
				}
				array5[0] = array[0];
				array5[2] = (array[2] - 2.0 * array[1] + array[0]) / 2.0;
				array5[1] = array[1] - array[0] - array5[2];
				array6[0] = array2[0];
				array6[2] = (array2[2] - 2.0 * array2[1] + array2[0]) / 2.0;
				array6[1] = array2[1] - array2[0] - array6[2];
				if (Moon == 8)
				{
					array7[0] = array3[0];
					array7[2] = (array3[2] - 2.0 * array3[1] + array3[0]) / 2.0;
					array7[1] = array3[1] - array3[0] - array7[2];
				}
				switch (Planet)
				{
				case 5:
					num2 = 98.47 / PlanetDistance;
					break;
				case 6:
					num2 = 83.18 / PlanetDistance;
					break;
				case 7:
					num2 = 34.28 / PlanetDistance;
					break;
				case 8:
					num2 = 36.56 / PlanetDistance;
					break;
				}
				for (int l = -1; l <= 1; l += 2)
				{
					double num16 = 0.0;
					double num17 = 0.0;
					num5 = 0;
					double num18;
					do
					{
						num18 = num16;
						double num19 = array5[0] + array5[1] * num18 + array5[2] * num18 * num18;
						double num20 = array6[0] + array6[1] * num18 + array6[2] * num18 * num18;
						double num21 = array5[1] + 2.0 * array5[2] * num18;
						double num22 = array6[1] + 2.0 * array6[2] * num18;
						double num23 = num2 * (num10 + num11 * Math.Cos(2.0 * Math.Atan(num20 / num19)) + num12 * Math.Cos(3.0 * Math.Atan(num20 / num19)));
						double num24 = num21 * num21 + num22 * num22;
						double num25 = Math.Abs(num19 * num22 - num20 * num21) / Math.Sqrt(num24);
						if (num23 > Math.Abs(num25))
						{
							double num26 = (double)l * Math.Sqrt((num23 * num23 - num25 * num25) / num24);
							if (double.IsNaN(num26))
							{
								num26 = 0.0;
							}
							num16 = num18 + num26 - (num19 * num21 + num20 * num22) / num24;
							num17 = 0.0;
							flag = false;
							if (Eclipse)
							{
								continue;
							}
							if (j == 0)
							{
								if (Math.Sign(num19) != Math.Sign(Math.Sin(PAlimb_deg / (180.0 / Math.PI))))
								{
									num17 = num7 * num26;
								}
							}
							else if (Math.Sign(num19) == Math.Sign(Math.Sin(PAlimb_deg / (180.0 / Math.PI))))
							{
								num17 = num7 * num26;
							}
						}
						else
						{
							num16 = num18 - (num19 * num21 + num20 * num22) / num24;
							flag = true;
							num5++;
							if (num5 > 10)
							{
								break;
							}
						}
					}
					while (Math.Abs(num18 - num16) > 0.0001);
					EventTimes[num4] = JD + (num14 + (num16 - 1.0 + num17) * num15) / 24.0 - num13;
					if (!flag)
					{
						EventTypes[num4] = 10 * Moon + (l + 1) / 2 + 2 * j + 4 * Convert.ToInt16(!Eclipse);
					}
					else
					{
						EventTypes[num4] = 0;
					}
					if (LocalEventsOnly)
					{
						double num27 = Math.Floor(JD - 0.5) + 0.5;
						if (!Utilities.ObjectUp_SunBelowCivil(num27, 24.0 * (JD - num27) + num14 + (num16 - 1.0 + num17) * num15, RA, Dec, SiteLongitude, SiteLatitude, RequireSunDown: true))
						{
							EventTypes[num4] = 0;
						}
					}
					num4++;
				}
				if (!(Planet == 6 && Moon == 8))
				{
					continue;
				}
				bool flag2 = false;
				double num28 = 5000.0;
				double num29 = 0.05;
				for (int num30 = 4; num30 >= 0; num30--)
				{
					for (double num31 = -2.5; num31 <= 3.5; num31 += num29)
					{
						double num19 = array5[0] + array5[1] * num31 + array5[2] * num31 * num31;
						double num20 = array6[0] + array6[1] * num31 + array6[2] * num31 * num31;
						double num32 = array4[num30] * (array7[0] + array7[1] * num31 + array7[2] * num31 * num31);
						double num33 = num32 * Math.Sin(Planetocentric_Latitude_deg / (180.0 / Math.PI));
						double num34 = Math.Sqrt(num19 * num19 / num32 / num32 + num20 * num20 / num33 / num33) - 1.0;
						if (num31 > -2.5)
						{
							if (num28 > 0.0 && num34 < 0.0)
							{
								EventTimes[num6] = JD + (num14 - num15 + (num31 + num29 * num34 / (num28 - num34)) * num15) / 24.0 - num13;
								EventTypes[num6] = 10 * Moon + 100 * (num30 + 1) + 2 * j + 4 * Convert.ToInt16(!Eclipse);
								array8[num6] = Math.Sign(num20) == Math.Sign(Planetocentric_Latitude_deg);
								if (num30 == 3 || num30 < 2)
								{
									EventTypes[num6]++;
								}
								num6++;
								flag2 = true;
							}
							if (num28 < 0.0 && num34 > 0.0)
							{
								EventTimes[num6] = JD + (num14 - num15 + (num31 + num29 * num34 / (num28 - num34)) * num15) / 24.0 - num13;
								EventTypes[num6] = 10 * Moon + 100 * (num30 + 1) + 1 + 2 * j + 4 * Convert.ToInt16(!Eclipse);
								if (num30 == 3 || num30 < 2)
								{
									EventTypes[num6]--;
								}
								num6++;
								flag2 = true;
								break;
							}
							if (num28 > 0.0 && num34 > num28)
							{
								break;
							}
						}
						num28 = num34;
					}
					if (!flag2)
					{
						break;
					}
				}
				if (!LocalEventsOnly)
				{
					continue;
				}
				for (int m = 4; m < 14 && EventTypes[m] != 0; m++)
				{
					double num35 = Math.Floor(EventTimes[m] - 0.5) + 0.5;
					if (!Utilities.ObjectUp_SunBelowCivil(num35, 24.0 * (EventTimes[m] - num35), RA, Dec, SiteLongitude, SiteLatitude, RequireSunDown: true))
					{
						EventTypes[m] = 0;
					}
				}
			}
			if (Eclipse)
			{
				if ((EventTimes[0] > EventTimes[2]) & (EventTimes[0] < EventTimes[3]))
				{
					EventTypes[0] = 0;
				}
				if ((EventTimes[1] > EventTimes[2]) & (EventTimes[1] < EventTimes[3]))
				{
					EventTypes[1] = 0;
				}
				if ((EventTimes[2] > EventTimes[0]) & (EventTimes[2] < EventTimes[1]))
				{
					EventTypes[2] = 0;
				}
				if ((EventTimes[3] > EventTimes[0]) & (EventTimes[3] < EventTimes[1]))
				{
					EventTypes[3] = 0;
				}
				if (Planet == 6 && Moon == 8)
				{
					for (int n = 4; n < 14; n++)
					{
						if (EventTypes[n] != 0)
						{
							if ((EventTimes[n] > EventTimes[0]) & (EventTimes[n] < EventTimes[1]))
							{
								EventTypes[n] = 0;
							}
							if ((EventTimes[n] > EventTimes[2]) & (EventTimes[n] < EventTimes[3]))
							{
								EventTypes[n] = 0;
							}
						}
					}
				}
			}
			if (!Eclipse && Planet == 6 && Moon == 8)
			{
				for (int num36 = 4; num36 < 14; num36++)
				{
					if (array8[num36] && EventTypes[num36] != 0)
					{
						int num37 = EventTypes[num36] % 10;
						if ((num37 == 4 || num37 == 5) && ((EventTimes[num36] > EventTimes[0]) & (EventTimes[num36] < EventTimes[1])))
						{
							EventTypes[num36] = 0;
						}
						if ((num37 == 6 || num37 == 7) && ((EventTimes[num36] > EventTimes[2]) & (EventTimes[num36] < EventTimes[3])))
						{
							EventTypes[num36] = 0;
						}
					}
				}
			}
			if (EventTimes[1] > EventTimes[2])
			{
				Utilities.Swap(ref EventTimes[1], ref EventTimes[2]);
				Utilities.Swap(ref EventTypes[1], ref EventTypes[2]);
			}
			if (EventTimes[0] > EventTimes[1])
			{
				Utilities.Swap(ref EventTimes[0], ref EventTimes[1]);
				Utilities.Swap(ref EventTypes[0], ref EventTypes[1]);
			}
			if (EventTimes[2] > EventTimes[3])
			{
				Utilities.Swap(ref EventTimes[2], ref EventTimes[3]);
				Utilities.Swap(ref EventTypes[2], ref EventTypes[3]);
			}
			if (EventTimes[1] > EventTimes[2])
			{
				Utilities.Swap(ref EventTimes[1], ref EventTimes[2]);
				Utilities.Swap(ref EventTypes[1], ref EventTypes[2]);
			}
			return result;
		}

		public static void SetPlanetPlusMoonPlotParameters(int Planet)
		{
			SetPlanetPlusMoonPlotParameters(Planet, 0);
		}

		public static void SetPlanetPlusMoonPlotParameters(int Planet, int Moon)
		{
			if (Moon > 0)
			{
				PlanetDiameterKM = SatelliteDiameter(Planet, Moon);
				PlanetRadius = (float)((double)(PlanetDiameterKM / 2f) / 6378.137) * 8.79414f;
				f = 0f;
				NumberOfRings = 0;
				InitialScale = 2f;
				return;
			}
			RingRadius[0] = 1f;
			switch (Planet)
			{
			case 1:
				PlanetRadius = 3.3638427f;
				f = 0f;
				NumberOfRings = 0;
				InitialScale = 2f;
				break;
			case 2:
				PlanetRadius = 8.440976f;
				f = 0f;
				NumberOfRings = 0;
				InitialScale = 2f;
				break;
			case 4:
				PlanetRadius = 4.678937f;
				f = 0f;
				NumberOfRings = 0;
				InitialScale = 2f;
				break;
			case 5:
				PlanetRadius = (float)Settings.Default.Radius_Jupiter / 6378.137f * 8.79414f;
				f = 0.064809f;
				NumberOfRings = 5;
				RingRadius[1] = 1.3988f;
				RingName[1] = "Halo-in";
				RingRadius[2] = 1.7177f;
				RingName[2] = "Halo-out";
				RingRadius[3] = 1.7205f;
				RingName[3] = "Main";
				RingRadius[4] = 1.8072f;
				RingName[4] = "Gossamer-";
				RingRadius[5] = 2.9961f;
				RingName[5] = "Gossamer+";
				InitialScale = 0.5f;
				break;
			case 6:
				PlanetRadius = (float)Settings.Default.Radius_Saturn / 6378.137f * 8.79414f;
				f = 0.09796f;
				NumberOfRings = 7;
				RingRadius[1] = 1.1117f;
				RingName[1] = "D-inner";
				RingRadius[2] = 1.236f;
				RingName[2] = "C-inner";
				RingRadius[3] = 1.5262f;
				RingName[3] = "B<>C";
				RingRadius[4] = 1.9496f;
				RingName[4] = "B-outer";
				RingRadius[5] = 2.0251f;
				RingName[5] = "A-inner";
				RingRadius[6] = 2.2694f;
				RingName[6] = "A-outer";
				RingRadius[7] = 2.3267f;
				RingName[7] = "F";
				InitialScale = 0.8f;
				break;
			case 7:
				PlanetRadius = (float)Settings.Default.Radius_Uranus / 6378.137f * 8.79414f;
				f = 0.02293f;
				NumberOfRings = 11;
				RingRadius[1] = 1.5494f;
				RingName[1] = "zeta";
				RingRadius[2] = 1.63688f;
				RingName[2] = "6";
				RingRadius[3] = 1.65245f;
				RingName[3] = "5";
				RingRadius[4] = 1.6656f;
				RingName[4] = "4";
				RingRadius[5] = 1.74962f;
				RingName[5] = "alpha";
				RingRadius[6] = 1.74962f;
				RingName[6] = "beta";
				RingRadius[7] = 1.84577f;
				RingName[7] = "eta";
				RingRadius[8] = 1.86338f;
				RingName[8] = "gamma";
				RingRadius[9] = 1.88975f;
				RingName[9] = "delta";
				RingRadius[10] = 1.9573f;
				RingName[10] = "lambda";
				RingRadius[11] = 2.00122f;
				RingName[11] = "epsilon";
				InitialScale = 4f;
				break;
			case 8:
				PlanetRadius = (float)Settings.Default.Radius_Neptune / 6378.137f * 8.79414f;
				f = 0.0259f;
				NumberOfRings = 5;
				RingRadius[1] = 1.692f;
				RingName[1] = "Galle";
				RingRadius[2] = 2.1483f;
				RingName[2] = "Leverrier";
				RingRadius[3] = 2.2371f;
				RingName[3] = "Lassell";
				RingRadius[4] = 2.326f;
				RingName[4] = "Arago";
				RingRadius[5] = 2.5412f;
				RingName[5] = "Adams";
				InitialScale = 2.5f;
				break;
			case 9:
				PlanetRadius = (float)Settings.Default.Radius_Pluto / 6378.137f * 8.79414f;
				f = 0f;
				NumberOfRings = 0;
				InitialScale = 25f;
				break;
			case 3:
				break;
			}
		}

		public static void PlotOrbits(Graphics formGraphics, float Width, float Height, double JD, int Planet, float PlotMagnification, bool AllMoons, bool ShowOrbits, bool ShowMinorRings, bool ShowMoons, bool ShowNames, bool ShowSatNumbers, bool BWflag, bool PrintFlag, bool PlotEarthForAsteroids, bool PlotOnEarthPlane)
		{
			bool[] array = new bool[20];
			bool flag = true;
			float[] array2 = new float[20];
			float[] array3 = new float[20];
			float num = 0f;
			float num2 = 0f;
			float num3 = 0f;
			float num4 = 0f;
			float num5 = 0f;
			float num6 = 0f;
			float num7 = 0f;
			float num8 = 0f;
			float num9 = 1f;
			double B = 0.0;
			double num10 = 0.0;
			double Sepn = 0.0;
			double PA = 0.0;
			double num11 = 1.0;
			double num12 = 0.0;
			string[] array4 = new string[20];
			List<MoonDistances> list = new List<MoonDistances>();
			Color[] array5 = new Color[10];
			Color[] array6 = new Color[2];
			Pen pen = new Pen(Color.Red);
			Pen pen2 = new Pen(Color.Gray);
			Font font = new Font("Courier New", 8f);
			Font font2 = new Font("Times New Roman", 7f, FontStyle.Regular);
			ArrayList arrayList = new ArrayList();
			if (Planet == 3 || Planet < 1 || Planet > 9)
			{
				return;
			}
			if (ShowSatNumbers && ShowNames)
			{
				ShowNames = false;
			}
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			Color color;
			Color color2;
			Color color3;
			Color color4;
			Color color5;
			Color color6;
			Brush brush;
			if (!BWflag && !PrintFlag)
			{
				array5[1] = Color.Chocolate;
				array5[2] = Color.White;
				array5[4] = Color.OrangeRed;
				array5[5] = Color.Gold;
				array5[6] = Color.Goldenrod;
				array5[7] = Color.CadetBlue;
				array5[8] = Color.Aquamarine;
				array5[9] = Color.MediumSlateBlue;
				array6[0] = Color.FromArgb(120, 120, 120);
				color = (array6[1] = Color.Olive);
				color2 = Color.FromArgb(150, 0, 0);
				color3 = Color.FromArgb(0, 150, 150);
				Color.FromArgb(150, 50, 250);
				color4 = Color.White;
				color5 = Color.FromArgb(210, 105, 30);
				Color.FromArgb(0, 200, 0);
				color6 = ((Planet != 2) ? Color.FromArgb(255 - array5[Planet].R, 255 - array5[Planet].G, 255 - array5[Planet].B) : Color.Peru);
				formGraphics.Clear(Color.Black);
				brush = new SolidBrush(Color.White);
			}
			else if (!BWflag && PrintFlag)
			{
				for (int i = 1; i < 10; i++)
				{
					array5[i] = Color.Black;
				}
				array6[0] = Color.Black;
				color = (array6[1] = Color.Black);
				color2 = Color.Black;
				color3 = Color.FromArgb(0, 50, 250);
				_ = Color.Black;
				color4 = Color.Black;
				color5 = Color.FromArgb(180, 180, 180);
				_ = Color.Black;
				color6 = Color.FromArgb(250, 100, 0);
				brush = new SolidBrush(Color.Black);
			}
			else
			{
				for (int j = 1; j < 10; j++)
				{
					array5[j] = Color.Black;
				}
				array6[0] = Color.Black;
				color = (array6[1] = Color.Black);
				color2 = Color.Black;
				color3 = Color.Black;
				_ = Color.Black;
				color4 = Color.Black;
				_ = Color.Black;
				color5 = Color.Gray;
				color6 = Color.Black;
				if (!PrintFlag)
				{
					formGraphics.Clear(Color.White);
				}
				brush = new SolidBrush(Color.Black);
			}
			Utilities.PlanetGeocentric(JD, Planet, out PlanetDistance, out var _, out var _, out var PhaseAngle_deg, out var _, out var PAlimb_deg, out var Planetocentric_Latitude_deg, out var PAPole_deg);
			Planetocentric_Latitude_deg /= 180.0 / Math.PI;
			PAPole_deg /= 180.0 / Math.PI;
			if (Planet == 6)
			{
				double Bprime = 0.0;
				flag = Math.Abs(B) > 4.0 || Utilities.SaturnRingsSunlit(JD, out B, out Bprime);
				B /= 180.0 / Math.PI;
				color = ((!flag) ? Color.Gray : array6[1]);
			}
			float num13 = Width / 2f;
			float num14 = 0.5f * Height;
			float num15 = Width / 50f * InitialScale * PlotMagnification;
			_ = 8.794143836182533 * (double)num15 / PlanetDistance;
			if (PlotEarthForAsteroids)
			{
				num12 = 24.0 * (JD - DisplayMPOccultations.EventJD) - DisplayMPOccultations.MidTime;
				num5 = (float)(num11 * (DisplayMPOccultations.DeltaX * num12 + DisplayMPOccultations.Delta2X * num12 * num12 + DisplayMPOccultations.Delta3X * num12 * num12 * num12));
				num6 = (float)(num11 * (DisplayMPOccultations.DeltaY * num12 + DisplayMPOccultations.Delta2Y * num12 * num12 + DisplayMPOccultations.Delta3Y * num12 * num12 * num12));
				if (DisplayMPOccultations.IsMoon)
				{
					SatelliteCoordinates(JD, DisplayMPOccultations.PlanetNumber, DisplayMPOccultations.MoonNumber, ViewedFromSun: false, ref Sepn, ref PA);
					num7 = (float)(Sepn * Math.Sin(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
					num8 = (float)(Sepn * Math.Cos(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
				}
				num11 = 8.794143836182533 * (double)num15 / PlanetDistance;
				num13 = ((!PlotOnEarthPlane) ? (num13 - (float)(num11 * (DisplayMPOccultations.Xatmin + (double)num5) - (double)num7)) : (num13 + (float)(num11 * (DisplayMPOccultations.Xatmin + (double)num5) - (double)num7)));
				num14 -= (float)(num11 * (DisplayMPOccultations.YatMin + (double)num6) - (double)num8);
				if (DisplayMPOccultations.PlanetNumber == 9 && ((DisplayMPOccultations.MoonNumber == 0) | (DisplayMPOccultations.MoonNumber == 1)))
				{
					SatelliteCoordinates(JD, 9, 1, ViewedFromSun: false, ref Sepn, ref PA);
					num7 = (float)(Sepn * Math.Sin(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
					num8 = (float)(Sepn * Math.Cos(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
					if (PlotOnEarthPlane)
					{
						num7 = 0f - num7;
					}
					if (PlotOnEarthPlane)
					{
						num13 += (float)(0.1139 * Sepn * Math.Sin(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
					}
					num13 -= (float)(0.1139 * Sepn * Math.Sin(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
					num14 -= (float)(0.1139 * Sepn * Math.Cos(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
				}
			}
			pen.Color = array5[Planet];
			float num20;
			if (f > 0f)
			{
				num = (float)((double)PlanetRadius / PlanetDistance * (double)num15);
				_ = 8.794143836182533 * (double)num15 / PlanetDistance;
				float num16 = (float)((double)(f / 2f) + (double)(f / 2f) * Math.Cos(2.0 * B));
				arrayList.Clear();
				for (int k = 0; k < 360; k += 3)
				{
					float num17 = (float)((double)num * (1.0 - (double)num16 / 2.0 + (double)num16 / 2.0 * Math.Cos((double)(2 * k) / (180.0 / Math.PI))));
					float num18 = (float)((double)num17 * Math.Cos((double)k / (180.0 / Math.PI)));
					float num19 = (float)((double)num17 * Math.Sin((double)k / (180.0 / Math.PI)));
					num20 = (float)((double)num13 - (double)num18 * Math.Cos(num10) - (double)num19 * Math.Sin(num10));
					float num21 = (float)((double)num14 - (double)num19 * Math.Cos(num10) + (double)num18 * Math.Sin(num10));
					arrayList.Add(new PointF(num20, num21));
					num3 = num20;
					num4 = num21;
				}
				PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
				if (!BWflag)
				{
					formGraphics.FillPolygon(new SolidBrush(array5[Planet]), points);
				}
				else
				{
					formGraphics.DrawPolygon(pen, points);
				}
			}
			else if (Planet < 9)
			{
				float num22 = (float)(180.0 - PAlimb_deg);
				if (PlotOnEarthPlane)
				{
					num22 = 360f - num22;
				}
				if (num22 < 0f)
				{
					num22 += 360f;
				}
				float num23 = num22 + 180f;
				num = (float)((double)PlanetRadius / PlanetDistance * (double)num15);
				pen.Color = color5;
				formGraphics.DrawArc(pen, num13 - num, num14 - num, 2f * num, 2f * num, num23, num23 - num22);
				arrayList.Clear();
				for (float num24 = 0f; num24 <= 180f; num24 += 5f)
				{
					float num18 = (float)((double)num * Math.Sin((double)num24 / (180.0 / Math.PI)));
					float num19 = (float)((double)num * Math.Cos((double)num24 / (180.0 / Math.PI)));
					double num25 = (double)(90f - num23) / (180.0 / Math.PI);
					num20 = (float)((double)num13 + (double)num18 * Math.Cos(num25) + (double)num19 * Math.Sin(num25));
					float num21 = (float)((double)num14 + (double)num19 * Math.Cos(num25) - (double)num18 * Math.Sin(num25));
					arrayList.Add(new PointF(num20, num21));
				}
				for (int l = 0; l <= 180; l += 5)
				{
					float num18 = (float)((double)num * Math.Sin((double)l / (180.0 / Math.PI)) * Math.Cos(PhaseAngle_deg / (180.0 / Math.PI)));
					float num19 = (float)((double)num * Math.Cos((double)l / (180.0 / Math.PI)));
					double num26 = (double)(90f - num22) / (180.0 / Math.PI);
					num20 = (float)((double)num13 + (double)num18 * Math.Cos(num26) + (double)num19 * Math.Sin(num26));
					float num21 = (float)((double)num14 + (double)num19 * Math.Cos(num26) - (double)num18 * Math.Sin(num26));
					arrayList.Add(new PointF(num20, num21));
				}
				if (arrayList.Count > 1)
				{
					PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
					if (!BWflag)
					{
						formGraphics.FillPolygon(new SolidBrush(array5[Planet]), points);
					}
					else
					{
						pen.Color = array5[Planet];
						brush = new SolidBrush(array5[Planet]);
						formGraphics.DrawPolygon(pen, points);
					}
				}
			}
			if (Planet > 3 && Planet < 9)
			{
				if (PlotOnEarthPlane)
				{
					PAPole_deg = Math.PI * 2.0 - PAPole_deg;
				}
				for (int m = 0; m <= NumberOfRings; m++)
				{
					if (m > 0 && !ShowMinorRings)
					{
						if (Planet != 6)
						{
							break;
						}
						if (m < 2)
						{
							continue;
						}
						if (m > 6)
						{
							break;
						}
					}
					if (m == 0)
					{
						pen.Color = color2;
					}
					else if (Planet != 6)
					{
						pen.Color = array6[0];
					}
					else
					{
						pen.Color = color;
					}
					arrayList.Clear();
					for (int n = 0; n <= 360; n += 4)
					{
						float num27 = RingRadius[m] * num;
						float num18 = (float)((double)num27 * Math.Cos((double)n / (180.0 / Math.PI)));
						float num19 = (float)((double)num27 * Math.Sin((double)n / (180.0 / Math.PI)) * Math.Sin(Planetocentric_Latitude_deg));
						float num28 = (float)Math.Sqrt(num18 * num18 + num19 * num19);
						num20 = (float)((double)num13 - (double)num18 * Math.Cos(PAPole_deg) - (double)num19 * Math.Sin(PAPole_deg));
						float num21 = (float)((double)num14 - (double)num19 * Math.Cos(PAPole_deg) + (double)num18 * Math.Sin(PAPole_deg));
						if ((num28 > num && num2 > num) || n > 180 || (n == 0 && m > 0))
						{
							arrayList.Add(new PointF(num20, num21));
						}
						else
						{
							if (num28 > num && num2 <= num)
							{
								float num29 = (num - num2) / (num28 - num2);
								arrayList.Add(new PointF(num3 + num29 * (num20 - num3), num4 + num29 * (num21 - num4)));
								arrayList.Add(new PointF(num20, num21));
							}
							if (num28 <= num && num2 > num)
							{
								float num29 = (num - num2) / (num28 - num2);
								arrayList.Add(new PointF(num3 + num29 * (num20 - num3), num4 + num29 * (num21 - num4)));
								if (arrayList.Count > 1)
								{
									PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
									formGraphics.DrawLines(pen, points);
								}
								arrayList.Clear();
							}
						}
						num3 = num20;
						num4 = num21;
						num2 = num28;
					}
					if (arrayList.Count > 1)
					{
						PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
						formGraphics.DrawLines(pen, points);
					}
				}
			}
			int num30;
			switch (Planet)
			{
			case 4:
				num30 = 2;
				break;
			case 5:
				num30 = 16;
				if (!AllMoons)
				{
					num30 = 4;
				}
				break;
			case 6:
				num30 = 14;
				if (!AllMoons)
				{
					num30 = 8;
				}
				break;
			case 7:
				num30 = 15;
				if (!AllMoons)
				{
					num30 = 5;
				}
				break;
			case 8:
				num30 = 2;
				if (!AllMoons)
				{
					num30 = 1;
				}
				break;
			case 9:
				num30 = 5;
				if (!AllMoons)
				{
					num30 = 1;
				}
				break;
			default:
				num30 = 0;
				break;
			}
			list.Clear();
			for (int num31 = 1; num31 <= num30; num31++)
			{
				if (Planet == 6 && num31 > 9 && num31 < 12)
				{
					continue;
				}
				SatelliteCoordinates(JD, Planet, num31, ViewedFromSun: false, ref Sepn, ref PA);
				B = BPlanet;
				num10 = PPlanet;
				if (PlotOnEarthPlane)
				{
					num10 = Math.PI * 2.0 - num10;
					PA = Math.PI * 2.0 - PA;
				}
				MoonDistances moonDistances = new MoonDistances();
				moonDistances.SatNum = num31;
				moonDistances.SatDistance = dDist;
				list.Add(moonDistances);
				double num32 = RMoon * (double)num15;
				double num33 = U - MoonTrueAnomaly;
				double EquationOfCentre = 0.0;
				double UnitaryRadiusVector = 1.0;
				array4[num31] = NameOfMoon;
				if (Planet == 9 && num31 == 1)
				{
					array4[0] = "Pluto";
				}
				if (ShowOrbits)
				{
					int num34 = 1;
					if (Planet == 9 && num31 == 1)
					{
						num34 = 2;
						num32 = 0.8835 * num32;
					}
					do
					{
						arrayList.Clear();
						pen.Color = color3;
						float[] array8 = (pen.DashPattern = new float[2] { 7f, 3f });
						int num35 = 5;
						if (e > 0.2)
						{
							num35 = 3;
						}
						if (e > 0.3)
						{
							num35 = 2;
						}
						if (e > 0.5)
						{
							num35 = 1;
						}
						for (int num36 = 0; num36 <= 361; num36 += num35)
						{
							TrueAnomaly_Radius((double)num36 / (180.0 / Math.PI) + num33 - UPlanet + Math.PI / 2.0, e, ref EquationOfCentre, ref UnitaryRadiusVector);
							float num18 = (float)(num32 * UnitaryRadiusVector * Math.Cos((double)num36 / (180.0 / Math.PI) + EquationOfCentre));
							float num19 = (float)(num32 * UnitaryRadiusVector * Math.Sin((double)num36 / (180.0 / Math.PI) + EquationOfCentre) * Math.Sin(B));
							float num28 = (float)Math.Sqrt(num18 * num18 + num19 * num19);
							if (PlotOnEarthPlane)
							{
								num18 = 0f - num18;
							}
							num20 = (float)((double)num13 - (double)num18 * Math.Cos(num10) - (double)num19 * Math.Sin(num10));
							float num21 = (float)((double)num14 - (double)num19 * Math.Cos(num10) + (double)num18 * Math.Sin(num10));
							if ((num28 > num && num2 > num) || num36 > 180)
							{
								arrayList.Add(new PointF(num20, num21));
								if ((num36 == 180) & (arrayList.Count > 1))
								{
									PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
									formGraphics.DrawLines(pen, points);
									pen.DashStyle = DashStyle.Solid;
									arrayList.Clear();
									arrayList.Add(new PointF(num20, num21));
								}
							}
							else
							{
								if (num28 > num && num2 <= num)
								{
									float num29 = (num - num2) / (num28 - num2);
									if (num36 > 0)
									{
										arrayList.Add(new PointF(num3 + num29 * (num20 - num3), num4 + num29 * (num21 - num4)));
									}
									arrayList.Add(new PointF(num20, num21));
								}
								if (num28 <= num && num2 > num)
								{
									float num29 = (num - num2) / (num28 - num2);
									arrayList.Add(new PointF(num3 + num29 * (num20 - num3), num4 + num29 * (num21 - num4)));
									if (arrayList.Count > 1)
									{
										PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
										formGraphics.DrawLines(pen, points);
									}
									arrayList.Clear();
								}
							}
							num3 = num20;
							num4 = num21;
							num2 = num28;
						}
						if (arrayList.Count > 1)
						{
							PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
							formGraphics.DrawLines(pen, points);
						}
						num34--;
						if (num34 == 1)
						{
							num32 = 0.1165 * RMoon * (double)num15;
						}
					}
					while (num34 > 0);
				}
				if (ShowMoons)
				{
					if (Planet == 9 && num31 == 1)
					{
						array2[num31] = (float)((double)num13 - 0.8861 * Sepn * Math.Sin(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
						array3[num31] = (float)((double)num14 - 0.8861 * Sepn * Math.Cos(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
						array[num31] = true;
						array2[0] = (float)((double)num13 + 0.1139 * Sepn * Math.Sin(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
						array3[0] = (float)((double)num14 + 0.1139 * Sepn * Math.Cos(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
						array[0] = true;
					}
					else
					{
						array[num31] = (Sepn * (double)num15 * (180.0 / Math.PI) * 3600.0 >= (double)num) | (dDist > 0.0);
						array2[num31] = (float)((double)num13 - Sepn * Math.Sin(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
						array3[num31] = (float)((double)num14 - Sepn * Math.Cos(PA) * (double)num15 * (180.0 / Math.PI) * 3600.0);
					}
				}
			}
			list.Sort();
			if (ShowMoons)
			{
				for (int num37 = 0; num37 <= num30; num37++)
				{
					if (Planet == 6 && num37 > 9 && num37 < 12)
					{
						continue;
					}
					int num38 = 0;
					if (Planet == 6 && num37 > 9)
					{
						num38 = list[num37 - 3].SatNum;
					}
					else if (num37 > 0)
					{
						num38 = list[num37 - 1].SatNum;
					}
					if (num37 == 0 && Planet < 9)
					{
						continue;
					}
					if (num37 > 0)
					{
						num9 = (float)((double)SatelliteDiameter(Planet, num38) / 6378.137 * 8.794143836182533 / 2.0 / PlanetDistance * (double)num15);
					}
					if (num9 < 2.5f)
					{
						if ((Planet == 5 && num38 < 5) || (Planet == 6 && num38 < 9) || (Planet == 7 && num38 < 6) || (Planet == 8 && num38 < 2) || (Planet == 9 && num38 == 4))
						{
							num9 = 2.5f;
						}
						else if (num9 < 1.5f)
						{
							num9 = 1.5f;
						}
					}
					if (!array[num38])
					{
						continue;
					}
					if (num37 == 0)
					{
						num = (float)((double)PlanetRadius / PlanetDistance * (double)num15);
						brush = new SolidBrush(array5[Planet]);
						formGraphics.FillEllipse(brush, array2[num38] - num, array3[num38] - num, 2f * num, 2f * num);
						continue;
					}
					formGraphics.FillEllipse(new SolidBrush(color4), array2[num38] - num9, array3[num38] - num9, 2f * num9, 2f * num9);
					if (num9 > 2f)
					{
						formGraphics.DrawEllipse(pen2, array2[num38] - num9, array3[num38] - num9, 2f * num9, 2f * num9);
					}
					if (ShowNames)
					{
						formGraphics.DrawString(array4[num38], font, new SolidBrush(color6), array2[num38] + num9, array3[num38] - 1f);
					}
					if (ShowSatNumbers)
					{
						formGraphics.DrawString(Convert.ToString(num38), font, brush, array2[num38] + num9, array3[num38] - 1f);
					}
				}
			}
			if (PlotEarthForAsteroids)
			{
				Maps.EarthGlobe(formGraphics, (float)num11, Width / 2f, 0.5f * Height, DisplayMPOccultations.SubstellarLongitude - num12 * 15.0, DisplayMPOccultations.FPlaneDec * (180.0 / Math.PI), DisplayMPOccultations.SubSolarLongitude_OfDate - 15.0 * num12, DisplayMPOccultations.SubSolarLatitude_OfDate, 0.0, 0.0, ShowSunLit: true, SiteCentered: false, PlotCities: false, FullResolution: false, BWflag, !PlotOnEarthPlane);
			}
			font = new Font("Courier New", 10f, FontStyle.Bold);
			formGraphics.DrawString("N", font, brush, Width / 2f, 2f);
			if (PlotOnEarthPlane)
			{
				formGraphics.DrawString("W", font, brush, 2f, Height / 2f);
			}
			else
			{
				formGraphics.DrawString("E", font, brush, 2f, Height / 2f);
			}
			font = new Font("Courier New", 8f);
			string text = Utilities.Date_from_JD(Math.Floor(JD - 0.5) + 0.5, 0) + Utilities.DEGtoDMS((JD - Math.Floor(JD - 0.5) - 0.5) * 24.0, 3, 0, MinutesOnly: true);
			formGraphics.DrawString(text, font, brush, 0.75f * Width, 2f);
			formGraphics.DrawString(text, font, brush, (Width - formGraphics.MeasureString(text, font).Width) / 2f, Height - 12f);
			formGraphics.DrawString("Occult " + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version, font2, brush, 12f, Height - 12f);
			num3 = 2f;
			int num39 = (int)((double)(Width / num15) / 5.0);
			if (num39 < 1)
			{
				num39 = 1;
			}
			string s = Convert.ToString(num39) + "\"";
			if (num39 > 10)
			{
				num39 -= num39 % 5;
				s = Convert.ToString(num39) + "\"";
			}
			if (num39 > 20)
			{
				num39 -= num39 % 10;
				s = Convert.ToString(num39) + "\"";
			}
			if (num39 > 120)
			{
				num39 -= num39 % 60;
				s = Convert.ToString(num39) + "'";
			}
			num20 = num3 + num15 * (float)num39;
			pen.Width = 1.5f;
			if (Planet == 5 || Planet == 6)
			{
				pen.Color = color6;
			}
			else
			{
				pen.Color = array5[Planet];
			}
			formGraphics.DrawLine(pen, num3, 10f, num20, 10f);
			formGraphics.DrawLine(pen, num3, 5f, num3, 15f);
			formGraphics.DrawLine(pen, num20, 5f, num20, 15f);
			pen.Width = 1f;
			brush = new SolidBrush(pen.Color);
			formGraphics.DrawString(s, font, brush, (num20 + num3) / 2f - 5f, 12f);
		}
	}
}
