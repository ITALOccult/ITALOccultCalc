using System.Collections.Generic;
using System.IO;

namespace Occult
{
	internal class AsteroidRings_All
	{
		public static string SourceFile = Utilities.AppPath + "\\Resource Files\\AsteroidRings.csv";

		public static List<AsteroidRingDetails> AsteroidRings = new List<AsteroidRingDetails>();

		public static void Fill_AllAsteroidRings()
		{
			AsteroidRings.Clear();
			if (File.Exists(SourceFile))
			{
				using StreamReader streamReader = new StreamReader(SourceFile);
				streamReader.ReadLine();
				while (!streamReader.EndOfStream)
				{
					AsteroidRingDetails asteroidRingDetails = new AsteroidRingDetails();
					string text = streamReader.ReadLine()!.Trim();
					if (text.Length == 0)
					{
						break;
					}
					asteroidRingDetails.ReadElementLine(text);
					AsteroidRings.Add(asteroidRingDetails);
				}
			}
			AsteroidRings.Sort();
		}

		internal static void GetRingDetails(int AsteroidNumber, out double RA_Pole_deg, out double Dec_Pole_deg, ref double[] RingRadius, out int NumberOfRings)
		{
			RingRadius = new double[5];
			for (int i = 0; i < 5; i++)
			{
				RingRadius[i] = 0.0;
			}
			RA_Pole_deg = (Dec_Pole_deg = (NumberOfRings = 0));
			if (AsteroidRings.Count < 1)
			{
				Fill_AllAsteroidRings();
			}
			for (int j = 0; j < AsteroidRings.Count; j++)
			{
				if (AsteroidRings[j].IDAsteroidNumber == AsteroidNumber)
				{
					RA_Pole_deg = AsteroidRings[j].RA_Pole;
					Dec_Pole_deg = AsteroidRings[j].Dec_Pole;
					NumberOfRings = AsteroidRings[j].NumberOfRings;
					RingRadius[0] = AsteroidRings[j].Radius1;
					RingRadius[1] = AsteroidRings[j].Radius2;
					RingRadius[2] = AsteroidRings[j].Radius3;
					RingRadius[3] = AsteroidRings[j].Radius4;
					RingRadius[4] = AsteroidRings[j].Radius5;
					break;
				}
			}
		}

		internal static int GetNumberOfRings(int AsteroidNumber)
		{
			if (AsteroidRings.Count < 1)
			{
				Fill_AllAsteroidRings();
			}
			for (int i = 0; i < AsteroidRings.Count; i++)
			{
				if (AsteroidRings[i].IDAsteroidNumber == AsteroidNumber)
				{
					return AsteroidRings[i].NumberOfRings;
				}
				if (AsteroidRings[i].IDAsteroidNumber == AsteroidNumber)
				{
					return 0;
				}
			}
			return 0;
		}
	}
}
