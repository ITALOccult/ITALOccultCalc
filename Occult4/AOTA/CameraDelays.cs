namespace AOTA
{
	internal class CameraDelays
	{
		private string camera = "";

		private double integrationDelay;

		private double nonintegrationDelay;

		private double multipleIntegrationDelay;

		internal string Camera
		{
			get
			{
				return camera;
			}
			set
			{
				camera = value;
			}
		}

		internal double IntegrationDelay
		{
			get
			{
				return integrationDelay;
			}
			set
			{
				integrationDelay = value;
			}
		}

		internal double NonIntegrationDelay
		{
			get
			{
				return nonintegrationDelay;
			}
			set
			{
				nonintegrationDelay = value;
			}
		}

		internal double MultipleIntegrationDelay
		{
			get
			{
				return multipleIntegrationDelay;
			}
			set
			{
				multipleIntegrationDelay = value;
			}
		}
	}
}
