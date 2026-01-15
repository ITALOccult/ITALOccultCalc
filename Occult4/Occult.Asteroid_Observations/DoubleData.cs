using System.Text;

namespace Occult.Asteroid_Observations
{
	internal class DoubleData
	{
		private double pa;

		private double separation;

		private double paUncertainty;

		private double separationUncertainty;

		private double centre_X;

		private double centre_Y;

		private double offset_X;

		private double offset_Y;

		private int solutionID;

		private bool companionSet;

		public bool Companion_Set
		{
			get
			{
				return companionSet;
			}
			set
			{
				companionSet = value;
			}
		}

		public double PA_Companion
		{
			get
			{
				return pa;
			}
			set
			{
				pa = value;
			}
		}

		public double Sep_Companion
		{
			get
			{
				if ((separation < 0.0) & (separation > -0.3))
				{
					return -0.3;
				}
				return separation;
			}
			set
			{
				separation = value;
			}
		}

		public double Sdev_PA_Companion
		{
			get
			{
				return paUncertainty;
			}
			set
			{
				paUncertainty = value;
			}
		}

		public double Sdev_Sep_Companion
		{
			get
			{
				return separationUncertainty;
			}
			set
			{
				separationUncertainty = value;
			}
		}

		public double Centre_X
		{
			get
			{
				return centre_X;
			}
			set
			{
				centre_X = value;
			}
		}

		public double Centre_Y
		{
			get
			{
				return centre_Y;
			}
			set
			{
				centre_Y = value;
			}
		}

		public double Offset_X
		{
			get
			{
				return offset_X;
			}
			set
			{
				offset_X = value;
			}
		}

		public double Offset_Y
		{
			get
			{
				return offset_Y;
			}
			set
			{
				offset_Y = value;
			}
		}

		public int SolutionID
		{
			get
			{
				return solutionID;
			}
			set
			{
				solutionID = value;
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("#{0,1:f0} ", SolutionID);
			stringBuilder.AppendFormat("{0,8:F1}", Sep_Companion);
			if ((Sdev_Sep_Companion > 0.0) & (Sdev_Sep_Companion < Sep_Companion * 0.7))
			{
				stringBuilder.AppendFormat("  ± {0,4:F1}mas", Sdev_Sep_Companion);
			}
			else
			{
				stringBuilder.Append("mas".PadRight(9));
			}
			stringBuilder.AppendFormat(", {0,5:F1}°", PA_Companion);
			if ((Sdev_PA_Companion > 0.0) & (Sdev_PA_Companion < 50.0))
			{
				stringBuilder.AppendFormat("  ± {0,4:F1}°", Sdev_PA_Companion);
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			return stringBuilder.ToString();
		}
	}
}
